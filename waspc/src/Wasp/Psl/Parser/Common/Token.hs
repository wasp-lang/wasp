-- This module is a copy-paste of the original `Text.Parsec.Token` module, with the following
-- changes:
-- 1. Removed support for multi-line comments.
-- 2. Changed the line comment definition to accept a Parser instead of just a String.
-- This way, we can accept arbitrary parsers for line comments, which is useful for our use case, as
-- just adding `//` as a line comment would make the parser ignore documentation comments `///`.
-- Original at
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/src/Text.Parsec.Token.html
-- -------------------------------------------------------------------------------------------------
{-# LANGUAGE PolymorphicComponents #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Text.Parsec.Token
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (uses local universal quantification: PolymorphicComponents)
--
-- A helper module to parse lexical elements (tokens). See 'makeTokenParser'
-- for a description of how to use it.
module Wasp.Psl.Parser.Common.Token
  ( LanguageDef,
    GenLanguageDef (..),
    TokenParser,
    GenTokenParser (..),
    makeTokenParser,
  )
where

import Control.Monad.Identity
import Data.Char (digitToInt, isAlpha, isSpace, toLower, toUpper)
import Data.List (sort)
import Data.Typeable (Typeable)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

-----------------------------------------------------------
-- Language Definition
-----------------------------------------------------------

type LanguageDef st = GenLanguageDef String st Identity

-- | The @GenLanguageDef@ type is a record that contains all parameterizable
-- features of the "Text.Parsec.Token" module. The module "Text.Parsec.Language"
-- contains some default definitions.
data GenLanguageDef s u m = LanguageDef
  { -- | Describes the start of a line comment.
    commentLine :: ParsecT s u m (),
    -- | Set to 'True' if the language supports nested block comments.
    nestedComments :: Bool,
    -- | This parser should accept any start characters of identifiers. For
    -- example @letter \<|> char \'_\'@.
    identStart :: ParsecT s u m Char,
    -- | This parser should accept any legal tail characters of identifiers.
    -- For example @alphaNum \<|> char \'_\'@.
    identLetter :: ParsecT s u m Char,
    -- | This parser should accept any start characters of operators. For
    -- example @oneOf \":!#$%&*+.\/\<=>?\@\\\\^|-~\"@
    opStart :: ParsecT s u m Char,
    -- | This parser should accept any legal tail characters of operators.
    -- Note that this parser should even be defined if the language doesn't
    -- support user-defined operators, or otherwise the 'reservedOp'
    -- parser won't work correctly.
    opLetter :: ParsecT s u m Char,
    -- | The list of reserved identifiers.
    reservedNames :: [String],
    -- | The list of reserved operators.
    reservedOpNames :: [String],
    -- | Set to 'True' if the language is case sensitive.
    caseSensitive :: Bool
  }
  deriving (Typeable)

-----------------------------------------------------------
-- A first class module: TokenParser
-----------------------------------------------------------

type TokenParser st = GenTokenParser String st Identity

-- | The type of the record that holds lexical parsers that work on
-- @s@ streams with state @u@ over a monad @m@.
data GenTokenParser s u m = TokenParser
  { -- | This lexeme parser parses a legal identifier. Returns the identifier
    -- string. This parser will fail on identifiers that are reserved
    -- words. Legal identifier (start) characters and reserved words are
    -- defined in the 'LanguageDef' that is passed to
    -- 'makeTokenParser'. An @identifier@ is treated as
    -- a single token using 'try'.
    identifier :: ParsecT s u m String,
    -- | The lexeme parser @reserved name@ parses @symbol
    -- name@, but it also checks that the @name@ is not a prefix of a
    -- valid identifier. A @reserved@ word is treated as a single token
    -- using 'try'.
    reserved :: String -> ParsecT s u m (),
    -- | This lexeme parser parses a legal operator. Returns the name of the
    -- operator. This parser will fail on any operators that are reserved
    -- operators. Legal operator (start) characters and reserved operators
    -- are defined in the 'LanguageDef' that is passed to
    -- 'makeTokenParser'. An @operator@ is treated as a
    -- single token using 'try'.
    operator :: ParsecT s u m String,
    -- | The lexeme parser @reservedOp name@ parses @symbol
    --  name@, but it also checks that the @name@ is not a prefix of a
    --  valid operator. A @reservedOp@ is treated as a single token using
    --  'try'.
    reservedOp :: String -> ParsecT s u m (),
    -- | This lexeme parser parses a single literal character. Returns the
    -- literal character value. This parsers deals correctly with escape
    -- sequences. The literal character is parsed according to the grammar
    -- rules defined in the Haskell report (which matches most programming
    -- languages quite closely).
    charLiteral :: ParsecT s u m Char,
    -- | This lexeme parser parses a literal string. Returns the literal
    -- string value. This parsers deals correctly with escape sequences and
    -- gaps. The literal string is parsed according to the grammar rules
    -- defined in the Haskell report (which matches most programming
    -- languages quite closely).
    stringLiteral :: ParsecT s u m String,
    -- | This lexeme parser parses a natural number (a positive whole
    -- number). Returns the value of the number. The number can be
    -- specified in 'decimal', 'hexadecimal' or
    -- 'octal'. The number is parsed according to the grammar
    -- rules in the Haskell report.
    natural :: ParsecT s u m Integer,
    -- | This lexeme parser parses an integer (a whole number). This parser
    -- is like 'natural' except that it can be prefixed with
    -- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
    -- number can be specified in 'decimal', 'hexadecimal'
    -- or 'octal'. The number is parsed according
    -- to the grammar rules in the Haskell report.
    integer :: ParsecT s u m Integer,
    -- | This lexeme parser parses a floating point value. Returns the value
    -- of the number. The number is parsed according to the grammar rules
    -- defined in the Haskell report.
    float :: ParsecT s u m Double,
    -- | This lexeme parser parses either 'natural' or a 'float'.
    -- Returns the value of the number. This parsers deals with
    -- any overlap in the grammar rules for naturals and floats. The number
    -- is parsed according to the grammar rules defined in the Haskell report.
    naturalOrFloat :: ParsecT s u m (Either Integer Double),
    -- | Parses a non-negative whole number in the decimal system. Returns the
    -- value of the number.
    decimal :: ParsecT s u m Integer,
    -- | Parses a non-negative whole number in the hexadecimal system. The
    -- number should be prefixed with \"x\" or \"X\". Returns the value of the
    -- number.
    hexadecimal :: ParsecT s u m Integer,
    -- | Parses a non-negative whole number in the octal system. The number
    -- should be prefixed with \"o\" or \"O\". Returns the value of the
    -- number.
    octal :: ParsecT s u m Integer,
    -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
    -- trailing white space.
    symbol :: String -> ParsecT s u m String,
    -- | @lexeme p@ first applies parser @p@ and then the 'whiteSpace'
    -- parser, returning the value of @p@. Every lexical
    -- token (lexeme) is defined using @lexeme@, this way every parse
    -- starts at a point without white space. Parsers that use @lexeme@ are
    -- called /lexeme/ parsers in this document.
    --
    -- The only point where the 'whiteSpace' parser should be
    -- called explicitly is the start of the main parser in order to skip
    -- any leading white space.
    --
    -- >    mainParser  = do{ whiteSpace
    -- >                     ; ds <- many (lexeme digit)
    -- >                     ; eof
    -- >                     ; return (sum ds)
    -- >                     }
    lexeme :: forall a. ParsecT s u m a -> ParsecT s u m a,
    -- | Parses any white space. White space consists of /zero/ or more
    -- occurrences of a 'space', a line comment or a block (multi
    -- line) comment. Block comments may be nested. How comments are
    -- started and ended is defined in the 'LanguageDef'
    -- that is passed to 'makeTokenParser'.
    whiteSpace :: ParsecT s u m (),
    -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
    -- returning the value of @p@.
    parens :: forall a. ParsecT s u m a -> ParsecT s u m a,
    -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
    -- \'}\'), returning the value of @p@.
    braces :: forall a. ParsecT s u m a -> ParsecT s u m a,
    -- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
    -- and \'>\'), returning the value of @p@.
    angles :: forall a. ParsecT s u m a -> ParsecT s u m a,
    -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
    -- and \']\'), returning the value of @p@.
    brackets :: forall a. ParsecT s u m a -> ParsecT s u m a,
    -- | DEPRECATED: Use 'brackets'.
    squares :: forall a. ParsecT s u m a -> ParsecT s u m a,
    -- | Lexeme parser |semi| parses the character \';\' and skips any
    -- trailing white space. Returns the string \";\".
    semi :: ParsecT s u m String,
    -- | Lexeme parser @comma@ parses the character \',\' and skips any
    -- trailing white space. Returns the string \",\".
    comma :: ParsecT s u m String,
    -- | Lexeme parser @colon@ parses the character \':\' and skips any
    -- trailing white space. Returns the string \":\".
    colon :: ParsecT s u m String,
    -- | Lexeme parser @dot@ parses the character \'.\' and skips any
    -- trailing white space. Returns the string \".\".
    dot :: ParsecT s u m String,
    -- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
    -- separated by 'semi'. Returns a list of values returned by
    -- @p@.
    semiSep :: forall a. ParsecT s u m a -> ParsecT s u m [a],
    -- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
    -- separated by 'semi'. Returns a list of values returned by @p@.
    semiSep1 :: forall a. ParsecT s u m a -> ParsecT s u m [a],
    -- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
    -- @p@ separated by 'comma'. Returns a list of values returned
    -- by @p@.
    commaSep :: forall a. ParsecT s u m a -> ParsecT s u m [a],
    -- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
    -- @p@ separated by 'comma'. Returns a list of values returned
    -- by @p@.
    commaSep1 :: forall a. ParsecT s u m a -> ParsecT s u m [a]
  }
  deriving (Typeable)

-----------------------------------------------------------
-- Given a LanguageDef, create a token parser.
-----------------------------------------------------------

-- | The expression @makeTokenParser language@ creates a 'GenTokenParser'
-- record that contains lexical parsers that are
-- defined using the definitions in the @language@ record.
--
-- The use of this function is quite stylized - one imports the
-- appropiate language definition and selects the lexical parsers that
-- are needed from the resulting 'GenTokenParser'.
--
-- >  module Main where
-- >
-- >  import Text.Parsec
-- >  import qualified Text.Parsec.Token as P
-- >  import Text.Parsec.Language (haskellDef)
-- >
-- >  -- The parser
-- >  ...
-- >
-- >  expr  =   parens expr
-- >        <|> identifier
-- >        <|> ...
-- >
-- >
-- >  -- The lexer
-- >  lexer       = P.makeTokenParser haskellDef
-- >
-- >  parens      = P.parens lexer
-- >  braces      = P.braces lexer
-- >  identifier  = P.identifier lexer
-- >  reserved    = P.reserved lexer
-- >  ...
makeTokenParser ::
  (Stream s m Char) =>
  GenLanguageDef s u m ->
  GenTokenParser s u m
makeTokenParser languageDef =
  TokenParser
    { identifier = identifier,
      reserved = reserved,
      operator = operator,
      reservedOp = reservedOp,
      charLiteral = charLiteral,
      stringLiteral = stringLiteral,
      natural = natural,
      integer = integer,
      float = float,
      naturalOrFloat = naturalOrFloat,
      decimal = decimal,
      hexadecimal = hexadecimal,
      octal = octal,
      symbol = symbol,
      lexeme = lexeme,
      whiteSpace = whiteSpace,
      parens = parens,
      braces = braces,
      angles = angles,
      brackets = brackets,
      squares = brackets,
      semi = semi,
      comma = comma,
      colon = colon,
      dot = dot,
      semiSep = semiSep,
      semiSep1 = semiSep1,
      commaSep = commaSep,
      commaSep1 = commaSep1
    }
  where
    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens p = between (symbol "(") (symbol ")") p
    braces p = between (symbol "{") (symbol "}") p
    angles p = between (symbol "<") (symbol ">") p
    brackets p = between (symbol "[") (symbol "]") p

    semi = symbol ";"
    comma = symbol ","
    dot = symbol "."
    colon = symbol ":"

    commaSep p = sepBy p comma
    semiSep p = sepBy p semi

    commaSep1 p = sepBy1 p comma
    semiSep1 p = sepBy1 p semi

    -----------------------------------------------------------
    -- Chars & Strings
    -----------------------------------------------------------
    charLiteral =
      lexeme
        ( between
            (char '\'')
            (char '\'' <?> "end of character")
            characterChar
        )
        <?> "character"

    characterChar =
      charLetter <|> charEscape
        <?> "literal character"

    charEscape = do _ <- char '\\'; escapeCode
    charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

    stringLiteral =
      lexeme
        ( do
            str <-
              between
                (char '"')
                (char '"' <?> "end of string")
                (many stringChar)
            return (foldr (maybe id (:)) "" str)
            <?> "literal string"
        )

    stringChar =
      do Just <$> stringLetter
        <|> stringEscape
        <?> "string character"

    stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape = do
      _ <- char '\\'
      do _ <- escapeGap; return Nothing
        <|> do _ <- escapeEmpty; return Nothing
        <|> do Just <$> escapeCode

    escapeEmpty = char '&'
    escapeGap = do
      _ <- many1 space
      char '\\' <?> "end of string gap"

    -- escape codes
    escapeCode =
      charEsc <|> charNum <|> charAscii <|> charControl
        <?> "escape code"

    charControl = do
      _ <- char '^'
      code <- upper
      return (toEnum (fromEnum code - fromEnum 'A' + 1))

    charNum = do
      code <-
        decimal
          <|> do _ <- char 'o'; number 8 octDigit
          <|> do _ <- char 'x'; number 16 hexDigit
      if code > 0x10FFFF
        then fail "invalid escape sequence"
        else return (toEnum (fromInteger code))

    charEsc = choice (map parseEsc escMap)
      where
        parseEsc (c, code) = do _ <- char c; return code

    charAscii = choice (map parseAscii asciiMap)
      where
        parseAscii (asc, code) = try (do _ <- string asc; return code)

    -- escape code tables
    escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
    asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes =
      [ "BS",
        "HT",
        "LF",
        "VT",
        "FF",
        "CR",
        "SO",
        "SI",
        "EM",
        "FS",
        "GS",
        "RS",
        "US",
        "SP"
      ]
    ascii3codes =
      [ "NUL",
        "SOH",
        "STX",
        "ETX",
        "EOT",
        "ENQ",
        "ACK",
        "BEL",
        "DLE",
        "DC1",
        "DC2",
        "DC3",
        "DC4",
        "NAK",
        "SYN",
        "ETB",
        "CAN",
        "SUB",
        "ESC",
        "DEL"
      ]

    ascii2 =
      [ '\BS',
        '\HT',
        '\LF',
        '\VT',
        '\FF',
        '\CR',
        '\SO',
        '\SI',
        '\EM',
        '\FS',
        '\GS',
        '\RS',
        '\US',
        '\SP'
      ]
    ascii3 =
      [ '\NUL',
        '\SOH',
        '\STX',
        '\ETX',
        '\EOT',
        '\ENQ',
        '\ACK',
        '\BEL',
        '\DLE',
        '\DC1',
        '\DC2',
        '\DC3',
        '\DC4',
        '\NAK',
        '\SYN',
        '\ETB',
        '\CAN',
        '\SUB',
        '\ESC',
        '\DEL'
      ]

    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    naturalOrFloat = lexeme natFloat <?> "number"

    float = lexeme floating <?> "float"
    integer = lexeme int <?> "integer"
    natural = lexeme nat <?> "natural"

    -- floats
    floating = do
      n <- decimal
      fractExponent n

    natFloat =
      do
        _ <- char '0'
        zeroNumFloat
        <|> decimalFloat

    zeroNumFloat =
      do
        n <- hexadecimal <|> octal
        return (Left n)
        <|> decimalFloat
        <|> fractFloat (0 :: Integer)
        <|> return (Left 0)

    decimalFloat = do
      n <- decimal
      option
        (Left n)
        (fractFloat n)

    fractFloat n = do
      f <- fractExponent n
      return (Right f)

    fractExponent n =
      do
        fract <- fraction
        expo <- option "" exponent'
        readDouble (show n ++ fract ++ expo)
        <|> do
          expo <- exponent'
          readDouble (show n ++ expo)
      where
        readDouble s =
          case reads s of
            [(x, "")] -> return x
            _ -> parserZero

    fraction =
      do
        _ <- char '.'
        digits <- many1 digit <?> "fraction"
        return ('.' : digits)
        <?> "fraction"

    exponent' =
      do
        _ <- oneOf "eE"
        sign' <- fmap (: []) (oneOf "+-") <|> return ""
        e <- decimal <?> "exponent"
        return ('e' : sign' ++ show e)
        <?> "exponent"

    -- integers and naturals
    int = do
      f <- lexeme sign
      f <$> nat

    sign =
      (char '-' >> return negate)
        <|> (char '+' >> return id)
        <|> return id

    nat = zeroNumber <|> decimal

    zeroNumber =
      do
        _ <- char '0'
        hexadecimal <|> octal <|> decimal <|> return 0
        <?> ""

    decimal = number 10 digit
    hexadecimal = do _ <- oneOf "xX"; number 16 hexDigit
    octal = do _ <- oneOf "oO"; number 8 octDigit

    number base baseDigit =
      do
        digits <- many1 baseDigit
        let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
        seq n (return n)

    -----------------------------------------------------------
    -- Operators & reserved ops
    -----------------------------------------------------------
    reservedOp name =
      lexeme $
        try $
          do
            _ <- string name
            notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)

    operator =
      lexeme $
        try $
          do
            name <- oper
            if isReservedOp name
              then unexpected ("reserved operator " ++ show name)
              else return name

    oper =
      do
        c <- opStart languageDef
        cs <- many (opLetter languageDef)
        return (c : cs)
        <?> "operator"

    isReservedOp name =
      isReserved (sort (reservedOpNames languageDef)) name

    -----------------------------------------------------------
    -- Identifiers & Reserved words
    -----------------------------------------------------------
    reserved name =
      lexeme $
        try $
          do
            _ <- caseString name
            notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)

    caseString name
      | caseSensitive languageDef = string name
      | otherwise = do walk name; return name
      where
        walk [] = return ()
        walk (c : cs) = do _ <- caseChar c <?> msg; walk cs

        caseChar c
          | isAlpha c = char (toLower c) <|> char (toUpper c)
          | otherwise = char c

        msg = show name

    identifier =
      lexeme $
        try $
          do
            name <- ident
            if isReservedName name
              then unexpected ("reserved word " ++ show name)
              else return name

    ident =
      do
        c <- identStart languageDef
        cs <- many (identLetter languageDef)
        return (c : cs)
        <?> "identifier"

    isReservedName name =
      isReserved theReservedNames caseName
      where
        caseName
          | caseSensitive languageDef = name
          | otherwise = map toLower name

    isReserved names name =
      scan names
      where
        scan [] = False
        scan (r : rs) = case compare r name of
          LT -> scan rs
          EQ -> True
          GT -> False

    theReservedNames
      | caseSensitive languageDef = sort reserved
      | otherwise = sort . map (map toLower) $ reserved
      where
        reserved = reservedNames languageDef

    -----------------------------------------------------------
    -- White space & symbols
    -----------------------------------------------------------
    symbol name =
      lexeme (string name)

    lexeme p =
      do x <- p; whiteSpace; return x

    -- whiteSpace
    whiteSpace = skipMany (simpleSpace <|> oneLineComment <?> "")

    simpleSpace =
      skipMany1 (satisfy isSpace)

    oneLineComment =
      do
        _ <- try (commentLine languageDef)
        skipMany (satisfy (/= '\n'))
        return ()
