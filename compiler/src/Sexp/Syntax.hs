module Sexp.Syntax where

import Data.Char qualified as C
import Data.List qualified as List
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Imports

data Sexp' a
  = Atom' {ann :: a, text :: Text}
  | List' {ann :: a, items :: [Sexp' a]}
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

pattern Atom :: Text -> Sexp
pattern Atom t = Atom' () t

pattern List :: [Sexp] -> Sexp
pattern List xs = List' () xs

{-# COMPLETE Atom, List #-}

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
