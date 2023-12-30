module Sexp.Syntax
  ( Ann (..),
    SexpP,
    pattern AnnP,
    pattern AtomP,
    pattern ListP,
    pattern Atom',
    pattern List',
    pattern Atom,
    pattern List,
    Fix (..),
    With (..),
    SexpF (..),
    pretty,
    prettyText,
    toText,
    toBuilder,
    parse,
    _Atom',
    _List',
    _Atom,
    _List,
    tokenize,
    SexpPos,
    Sexp,
  )
where

import Data.Char qualified as C
import Data.Functor.Compose
import Data.List qualified as List
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import GHC.Records (HasField (..))
import Imports
import Prettyprinter ((<+>))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as P.Render.Text

data SexpF a
  = AtomF Text
  | ListF [a]
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

data Ann
  = Line
  | IndentLine

type SexpP = Fix (Compose (Either Ann) SexpF)

pattern AtomP :: Text -> SexpP
pattern AtomP t = In (Compose (Right (AtomF t)))

pattern ListP :: [SexpP] -> SexpP
pattern ListP xs = In (Compose (Right (ListF xs)))

pattern AnnP :: Ann -> SexpP
pattern AnnP a = In (Compose (Left a))

{-# COMPLETE AtomP, ListP, AnnP #-}

instance IsString SexpP where
  fromString = AtomP . fromString

newtype Fix f = In {unFix :: (f (Fix f))}

data With a x = With {ann :: a, value :: x}

type Sexp' a = Fix (Compose (With a) SexpF)

pattern Atom' :: a -> Text -> Sexp' a
pattern Atom' a t = In (Compose (With a (AtomF t)))

pattern List' :: a -> [Sexp' a] -> Sexp' a
pattern List' a xs = In (Compose (With a (ListF xs)))

{-# COMPLETE Atom', List' #-}

pattern Atom :: Text -> Sexp
pattern Atom t = Atom' () t

pattern List :: [Sexp] -> Sexp
pattern List xs = List' () xs

{-# COMPLETE Atom, List #-}

instance HasField "ann" (Sexp' a) a where
  getField (Atom' ann _) = ann
  getField (List' ann _) = ann

type Sexp = Sexp' ()

type SexpPos = Sexp' Int

instance IsString Sexp where
  fromString = Atom . fromString

_Atom' :: Prism' (Sexp' a) (a, Text)
_Atom' = prism' (uncurry Atom') $ \case
  Atom' p t -> Just (p, t)
  _ -> Nothing

_List' :: Prism' (Sexp' a) (a, [Sexp' a])
_List' = prism' (uncurry List') $ \case
  List' p xs -> Just (p, xs)
  _ -> Nothing

_Atom :: Prism' Sexp Text
_Atom = prism' Atom $ \case
  Atom t -> Just t
  _ -> Nothing

_List :: Prism' Sexp [Sexp]
_List = prism' List $ \case
  List xs -> Just xs
  _ -> Nothing

toText :: Sexp -> Text
toText = TB.runBuilder . toBuilder

toBuilder :: Sexp -> TB.Builder
toBuilder (Atom text) = text.tb
toBuilder (List items) = "(".tb <> mconcat (List.intersperse " ".tb (toBuilder <$> items)) <> ")".tb

data Token
  = LParen
  | RParen
  | Thing Text
  deriving (Show, Eq)

data TokenPos = TokenPos {pos :: Int, token :: Token}

tokenize :: Text -> [TokenPos]
tokenize = go 1
  where
    go !pos !t =
      case T.uncons t of
        Nothing -> []
        Just (c, rest) ->
          case c of
            '(' -> TokenPos pos LParen : go (pos + 1) rest
            ')' -> TokenPos pos RParen : go (pos + 1) rest
            c | C.isSpace c -> go (pos + 1) rest
            _ -> TokenPos pos (Thing atom) : go (pos + T.length atom) newRest
              where
                (atom, newRest) = T.break atomEnd t

atomEnd :: Char -> Bool
atomEnd c = C.isSpace c || c == '(' || c == ')'

parse :: Text -> Either Text [SexpPos]
parse = fmap fst . parse' . tokenize

parse' :: [TokenPos] -> Either Text ([SexpPos], [TokenPos])
parse' = pManySexp
  where
    pManySexp [] = pure ([], [])
    pManySexp ts@((TokenPos _ t) : _) = case t of
      RParen -> pure ([], ts)
      _ -> do
        (sexp, ts) <- pSexp ts
        (sexps, ts) <- pManySexp ts
        pure (sexp : sexps, ts)

    pSexp [] = undefined
    pSexp ts@((TokenPos pos t) : ts') = case t of
      LParen -> pList ts
      Thing t -> pure (Atom' pos t, ts')
      _ -> error "unexpected"

    pList [] = undefined
    pList ((TokenPos pos t) : ts) = case t of
      LParen -> do
        (sexps, ts) <- pManySexp ts
        case ts of
          (TokenPos _ RParen) : ts -> pure (List' pos sexps, ts)
          _ -> Left "missing close paren".t
      _ -> error "need lparen"

type Doc = P.Doc Void

prettyText :: SexpP -> Text
prettyText = P.Render.Text.renderStrict . P.layoutPretty P.defaultLayoutOptions . pretty

pretty :: SexpP -> Doc
pretty (AnnP _) = mempty
pretty (AtomP t) = P.pretty t
pretty (ListP xs) = P.parens (prettyList xs)

prettyAnn :: Ann -> Doc -> Doc
prettyAnn ann doc = case ann of
  Line -> P.line <> doc
  IndentLine -> P.nest 2 (P.line <> doc)

prettyList :: [SexpP] -> Doc
prettyList (AnnP ann : xs) = prettyAnn ann (prettyList xs)
prettyList (x : AnnP ann : xs) = pretty x <> prettyAnn ann (prettyList xs)
prettyList (x : []) = pretty x
prettyList (x : xs) = pretty x <+> prettyList xs
prettyList [] = mempty
