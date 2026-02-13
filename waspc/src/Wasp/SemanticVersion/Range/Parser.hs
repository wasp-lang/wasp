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

parseRange :: String -> Either ParseError Range
parseRange = P.parse rangeParser ""

rangeParser :: Parsec String () Range
rangeParser = Range <$> (comparatorSetParser `P.sepBy1` P.try logicalOrP)
  where
    logicalOrP :: Parsec String () ()
    logicalOrP = P.spaces *> P.string "||" *> P.spaces

    -- A comparator set is either:
    -- 1. A single hyphen range (cannot be combined with other comparators).
    -- 2. One or more simple comparators (primitive, caret, tilde, x-range).
    comparatorSetParser :: Parsec String () ComparatorSet
    comparatorSetParser =
      P.choice
        [ ComparatorSet . pure <$> P.try hyphenRangeParser,
          simpleComparatorSetParser
        ]

    simpleComparatorSetParser :: Parsec String () ComparatorSet
    simpleComparatorSetParser = do
      comparators <- simpleComparatorParser `P.sepBy1` P.try spaceSeparator
      case NE.nonEmpty comparators of
        Just neComps -> return $ ComparatorSet neComps
        Nothing -> fail "Expected at least one comparator"
      where
        -- Space separator, but not before || or at end
        spaceSeparator = do
          _ <- P.many1 (P.char ' ')
          P.notFollowedBy (P.string "||")
          P.notFollowedBy P.eof
          return ()

    -- Simple comparators are all comparators except hyphen ranges.
    simpleComparatorParser :: Parsec String () Comparator
    simpleComparatorParser =
      P.choice
        [ P.try tildeParser,
          P.try caretParser,
          P.try primitiveParser,
          xRangeParser
        ]
        -- TODO: test this erorr message
        <?> "comparator"

    hyphenRangeParser :: Parsec String () Comparator
    hyphenRangeParser = HyphenRange <$> partialVersionParser <*> (P.space *> P.char '-' *> P.space *> partialVersionParser)

    tildeParser :: Parsec String () Comparator
    tildeParser = ApproximatelyEquvivalentTo <$> (P.char '~' *> partialVersionParser)

    caretParser :: Parsec String () Comparator
    caretParser = BackwardsCompatibleWith <$> (P.char '^' *> partialVersionParser)

    xRangeParser :: Parsec String () Comparator
    xRangeParser = XRange <$> partialVersionParser

    primitiveParser :: Parsec String () Comparator
    primitiveParser = PrimitiveComparator <$> primitiveOperatorParser <*> partialVersionParser

    primitiveOperatorParser :: Parsec String () PrimitiveOperator
    primitiveOperatorParser =
      P.choice
        [ LessThanOrEqual <$ P.try (P.string "<="),
          GreaterThanOrEqual <$ P.try (P.string ">="),
          LessThan <$ P.char '<',
          GreaterThan <$ P.char '>',
          Equal <$ P.char '='
        ]
        <?> "operator"
