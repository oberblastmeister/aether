{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Cp.Parser (parse, showError) where

import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Cp.Syntax
import Data.Char qualified as Char
import Data.Foldable (foldl')
import Data.Text qualified as T
import Imports hiding (noneOf)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

type Parser = Parsec CustomError Text

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme = L.lexeme ws

symbol s = lexeme (C.string s)

char c = lexeme (C.char c)

delim d d' p = char d *> p <* char d'

parens = delim '(' ')'

braces = delim '{' '}'

brackets = delim '[' ']'

scientific = lexeme L.scientific

decimal = lexeme L.decimal

signed = L.signed ws

data CustomError = CustomError Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (CustomError e) = e.s
  errorComponentLen (CustomError e) = T.length e

structOpen :: Parser ()
structOpen = try do
  C.char '.'
  char '{'
  pure ()

isKeyword :: Text -> Bool
-- isKeyword x = x == "return" || x == "let" || x == "fn" || x == "struct" || x == "enum" || x == "union"
isKeyword _ = False

isIdentStart c = Char.isAlpha c || c == '_'

isIdentContinue c = Char.isAlphaNum c || c == '_'

pIdent :: Parser Text
pIdent = dbg "ident" $ try do
  _ <- lookAhead $ satisfy $ isIdentStart
  x <- takeWhile1P Nothing isIdentContinue
  guard (not (isKeyword x))
  x <$ ws

dotIdent :: Parser Text
dotIdent = try do
  C.char '.'
  pIdent

pHex :: Parser NumLiteral
pHex = try do
  _ <- char '0'
  _ <- char 'x' <|> char 'X'
  hexText <-
    takeWhile1P Nothing Char.isHexDigit
      <?> "hexadecimal integer"
  pure $ Hex hexText (mkNum hexText)
  where
    mkNum = foldl' step 0 . T.unpack
    step a c = a * 16 + fromIntegral (Char.digitToInt c)

customError :: Text -> Parser a
customError = customFailure . CustomError

pDecimal :: Parser Integer
pDecimal = dbg "decimal" do
  n <- signed decimal
  traceM "after decimal"
  pure n

-- case Scientific.floatingOrInteger n of
--   Left (_d :: Double) -> customError "got float instead of integer"
--   Right x -> pure x

pKeyword :: Text -> Parser ()
pKeyword kw = do
  _ <- C.string kw
  (takeWhile1P Nothing Char.isAlphaNum *> empty) <|> ws

pArray :: Parser ArrayType
pArray = do
  size <- brackets pDecimal
  ty <- pType
  pure ArrayType {size, ty}

pStructLiteral :: Parser (Expr Parsed)
pStructLiteral = do
  structOpen
  fields <-
    sepEndBy
      ( do
          field <- optional (dotIdent <* char '=')
          expr <- pExpr
          pure (field, expr)
      )
      (char ',')
  char '}'
  pure $ BraceLiteral fields

pEnumLiteral :: Parser (Expr Parsed)
pEnumLiteral = do
  tag <- dotIdent
  pure $ EnumLiteral tag ()

pType :: Parser Type
pType = dbg "type" do
  (PrimBool <$ pKeyword "bool")
    <|> (Pointer <$> (char '*' *> pType))
    <|> (Void <$ pKeyword "void")
    <|> (Array <$> pArray)
    <|> (PrimInt <$> pIntType)
    <|> customError "invalid type"

pParam :: Parser (Text, Type)
pParam = ((,) <$> pIdent <*> (char ':' *> pType))

pParamList :: Parser [(Text, Type)]
pParamList = sepEndBy pParam (char ',')

pStruct :: Parser (Decl Parsed)
pStruct = do
  pKeyword "struct"
  name <- pIdent
  fields <- braces pParamList
  let info = StructInfo {fields, fieldMap = fields.hm}
  pure $ Struct name info

pEnumTag :: Parser (Text, Maybe NumLiteral)
pEnumTag = do
  name <- pIdent
  val <- optional do
    char '='
    pNumLiteral
  pure (name, val)

pIntType :: Parser IntType
pIntType =
  (pKeyword "u8" $> u U8)
    <|> (pKeyword "u16" $> u U16)
    <|> (pKeyword "u32" $> u U32)
    <|> (pKeyword "u64" $> u U64)
    <|> (pKeyword "i8" $> i U8)
    <|> (pKeyword "i16" $> i U16)
    <|> (pKeyword "i32" $> i U32)
    <|> (pKeyword "i64" $> i U64)
  where
    u s = IntType s Unsigned
    i s = IntType s Signed

pEnum :: Parser (Decl Parsed)
pEnum = do
  pKeyword "enum"
  repType <- parens pIntType
  name <- pIdent
  tags <- braces do
    sepEndBy1 pEnumTag (char ',')
  let info = EnumInfo {repType, tags}
  pure $ Enum name info

pAt :: Parser Text
pAt = try do
  C.char '@'
  pIdent

pBuiltinArg :: Parser (BuiltinArg Parsed)
pBuiltinArg =
  (TypeArg <$> pType)
    <|> (ExprArg <$> pExpr)
    <|> (IdentArg <$> (C.char '~' *> pIdent))
    <|> customError "builtin argument"

pBuiltin :: Parser (Expr Parsed)
pBuiltin = do
  ident <- pAt
  argList <- parens $ sepEndBy1 pBuiltinArg (char ',')
  pure $ Builtin $ BuiltinParsed ident argList

pBody :: Parser [Stmt Parsed]
pBody = dbg "body" do
  braces $ many do
    notFollowedBy $ void (char '}') <|> eof
    dbg "stmt" pStmt

pIfCont :: Parser (IfCont Parsed)
pIfCont =
  ( do
      try do
        pKeyword "else"
        pKeyword "if"
      cond <- pExpr
      body <- pBody
      cont <- pIfCont
      pure ElseIf {cond, body, cont}
  )
    <|> ( do
            pKeyword "else"
            body <- pBody
            pure Else {body}
        )
    <|> (pure NoIfCont)

pIf :: Parser (Stmt Parsed)
pIf = do
  pKeyword "if"
  cond <- pExpr
  body <- pBody
  cont <- pIfCont
  pure $ If cond body cont

pLoop :: Parser (Stmt Parsed)
pLoop = do
  pKeyword "loop"
  body <- pBody
  pure $ Loop body

pReturn :: Parser (Stmt Parsed)
pReturn = do
  pKeyword "return"
  ret <- ((Just <$> pExpr) <* char ';') <|> (Nothing <$ char ';')
  pure $ Return ret

pSet :: Parser (Stmt Parsed)
pSet = do
  pKeyword "set"
  e <- pExpr
  char '='
  e' <- pExpr
  char ';'
  pure $ Set e e'

pStmt :: Parser (Stmt Parsed)
pStmt = do
  pReturn
    <|> pLet
    <|> pSet
    <|> pIf
    <|> pLoop
    <|> (ExprStmt <$> (pExpr <* char ';'))

pGetField :: Parser (Expr Parsed -> Expr Parsed)
pGetField = do
  C.char '.'
  name <- pIdent
  pure (`GetField` name)

pPlace :: Parser (Expr Parsed -> Expr Parsed)
pPlace = (Deref <$ pKeyword ".*") <|> pGetField

manyPostfix :: Text -> (a -> a) -> Operator Parser a
manyPostfix name f = Postfix (foldr1 (.) <$> some (f <$ symbol name))

pArgList :: Parser [(Expr Parsed)]
pArgList = parens $ sepEndBy (do notFollowedBy (char ')'); pExpr) (char ',')

pEither :: Parser a -> Parser b -> Parser (Either a b)
pEither p p' = (Left <$> p) <|> (Right <$> p')

pCall :: Parser (Expr Parsed)
pCall = do
  traceM "in call"
  x <- pIdent
  traceM "after ident call"
  ( do
      argList <- pArgList
      pure $ Call x argList
    )
    <|> (pure $ Var x)

pEscape :: Parser String
pEscape = do
  d <- char '\\'
  c <- oneOf ("\\\"'nt" :: String)
  pure [d, c]

pStringChar :: Parser String
pStringChar = pEscape <|> pure <$> (noneOf ("\"\n" :: String))

pString :: Parser Text
pString = do
  char '"'
  cs <- many pStringChar
  char '"'
  pure $ (concat cs).t

pChar :: Parser Text
pChar = do
  char '\''
  c <- pStringChar
  char '\''
  pure $ c.t

pBool :: Parser Bool
pBool = (True <$ pKeyword "true") <|> (False <$ pKeyword "false")

pNumLiteral :: Parser NumLiteral
pNumLiteral = pHex <|> (Decimal <$> pDecimal)

pLiteral :: Parser (Literal Parsed)
pLiteral =
  (Bool <$> pBool)
    <|> (String <$> pString)
    <|> (Char <$> pChar)
    <|> (Num <$> pNumLiteral <*> pure ())

pLet :: Parser (Stmt Parsed)
pLet = do
  pKeyword "let"
  name <- pIdent
  ty <- optional do
    char ':'
    pType
  char '='
  expr <- pExpr
  char ';'
  pure $ Let name ty expr ()

pExpr :: Parser (Expr Parsed)
pExpr = pExpr' >>= rest
  where
    pExpr' =
      (NullPtr <$ pKeyword "nullptr")
        <|> (Ref <$> (char '&' *> pExpr))
        <|> dbg "literal" (Literal <$> pLiteral)
        <|> pStructLiteral
        <|> pEnumLiteral
        <|> pBuiltin
        <|> pCall
        <|> customError "expression"

    rest e =
      ( do
          f <- pPlace
          rest (f e)
      )
        <|> pure e

-- pPlaces :: Parser PlaceContext
-- pPlaces = go CxHole
--   where
--     go p =
--       ( do
--           f <- pPlace
--           go (f p)
--       )
--         <|> pure p

pFn :: Parser (Decl Parsed)
pFn = dbg "fn" do
  pKeyword "fn"
  name <- pIdent
  params <- parens pParamList
  returnType <- pType
  body <- (Just <$> pBody) <|> (Nothing <$ char ';')
  let fnType = FnType {params = params, returnType}
  let info = FnInfo fnType (fmap fst params) body
  pure $ Fn name info

pDecl :: Parser (Decl Parsed)
pDecl = dbg "decl" do
  pFn
    <|> pEnum
    <|> pStruct

contents :: Parser a -> Parser a
contents p = ws *> p <* eof

parseFull :: Parser a -> Text -> Either (ParseErrorBundle Text CustomError) a
parseFull p s = P.parse (contents p) "<stdin>" s

showError :: (ParseErrorBundle Text CustomError) -> String
showError e = errorBundlePretty e

pProgram :: Parser (ProgramParsed)
pProgram = dbg "program" do
  ProgramParsed <$> many pDecl

parse :: Text -> Either (ParseErrorBundle Text CustomError) (ProgramParsed)
parse t = parseFull pProgram t
