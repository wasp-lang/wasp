module Wasp.Psl.Parser.AttachedComment
  ( withAttachedComments,
  )
where

import Control.Monad (void)
import Data.Maybe (maybeToList)
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Wasp.Psl.Ast.AttachedComment
  ( AttachedComment (AttachedComment),
    AttachedCommentLocation (EndOfLine, LineBefore),
  )
import Wasp.Psl.Parser.Common
  ( symbol,
    whiteSpace,
  )

withAttachedComments :: Parser ([AttachedComment] -> a) -> Parser a
withAttachedComments innerParser = do
  lineBeforeComments <- Parsec.optionMaybe attachedCommentLineBlockParser
  innerResult <- innerParser
  whiteSpace
  endOfLineComments <- Parsec.optionMaybe attachedCommentLineBlockParser

  let comments =
        toAttachedComment LineBefore lineBeforeComments
          <> toAttachedComment EndOfLine endOfLineComments

  return $ innerResult comments
  where
    toAttachedComment :: AttachedCommentLocation -> Maybe [String] -> [AttachedComment]
    toAttachedComment location = maybeToList . fmap (AttachedComment location . unlines)

attachedCommentLineBlockParser :: Parser [String]
attachedCommentLineBlockParser = Parsec.many $ do
  Parsec.optional whiteSpace
  line <- attachedCommentLineParser
  Parsec.optional whiteSpace
  return line

attachedCommentLineParser :: Parser String
attachedCommentLineParser = do
  whiteSpace
  void $ symbol "///"
  Parsec.many (Parsec.noneOf "\n")
