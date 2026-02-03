module Wasp.SemanticVersion.Range.Parser
  ( parseRange,
    rangeParser,
  )
where

import qualified Data.List.NonEmpty as NE
import Text.Parsec (ParseError, Parsec, (<?>))
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
  )
import Wasp.SemanticVersion.ComparatorSet (ComparatorSet (..))
import Wasp.SemanticVersion.PartialVersion (partialVersionParser)
import Wasp.SemanticVersion.Range (Range (..))

-- | Parse a semver range string.
-- Examples: ">=1.0.0 <2.0.0", "^1.2.3", "~1.2", "1.2.x", "1.2.3 - 2.3.4", ">=1.0.0 || <0.5.0"
parseRange :: String -> Either ParseError Range
parseRange = P.parse rangeParser ""

rangeParser :: Parsec String () Range
rangeParser = Range <$> (P.spaces *> comparatorSetParser `P.sepBy1` orSeparator <* P.spaces <* P.eof)
  where
    orSeparator = P.try $ P.spaces *> P.string "||" *> P.spaces

comparatorSetParser :: Parsec String () ComparatorSet
comparatorSetParser = do
  comps <- comparatorParser `P.sepBy1` P.try spaceSeparator
  case NE.nonEmpty comps of
    Just neComps -> return $ ComparatorSet neComps
    Nothing -> fail "Expected at least one comparator"
  where
    -- Space separator, but not before || or at end
    spaceSeparator = do
      _ <- P.many1 (P.char ' ')
      P.notFollowedBy (P.string "||")
      P.notFollowedBy P.eof
      return ()

comparatorParser :: Parsec String () Comparator
comparatorParser =
  P.choice
    [ P.try hyphenRangeParser,
      P.try tildeParser,
      P.try caretParser,
      P.try primitiveParser,
      xRangeParser
    ]
    <?> "comparator"

hyphenRangeParser :: Parsec String () Comparator
hyphenRangeParser = HyphenRange <$> partialVersionParser <*> (P.spaces *> P.char '-' *> P.spaces *> partialVersionParser)

tildeParser :: Parsec String () Comparator
tildeParser = ApproximatelyEquvivalentTo <$> (P.char '~' *> P.spaces *> partialVersionParser)

caretParser :: Parsec String () Comparator
caretParser = BackwardsCompatibleWith <$> (P.char '^' *> P.spaces *> partialVersionParser)

primitiveParser :: Parsec String () Comparator
primitiveParser = PrimitiveComparator <$> operatorParser <*> (P.spaces *> partialVersionParser)

operatorParser :: Parsec String () PrimitiveOperator
operatorParser =
  P.choice
    [ LessThanOrEqual <$ P.try (P.string "<="),
      GreaterThanOrEqual <$ P.try (P.string ">="),
      LessThan <$ P.char '<',
      GreaterThan <$ P.char '>',
      Equal <$ P.char '='
    ]
    <?> "operator"

xRangeParser :: Parsec String () Comparator
xRangeParser = XRange <$> partialVersionParser
