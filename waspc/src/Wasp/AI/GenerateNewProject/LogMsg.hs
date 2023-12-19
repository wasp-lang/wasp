module Wasp.AI.GenerateNewProject.LogMsg
  ( LogMsg,
    styled,
    toPlainString,
    toTermString,
    fromText,
    Style (..),
  )
where

import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Wasp.Util.Terminal as Term

data LogMsg = Plain String | Concat [LogMsg] | Styled Style LogMsg
  deriving (Show, Eq)

data Style = Important | Generating | Fixing | Error | Custom [Term.Style]
  deriving (Show, Eq)

instance IsString LogMsg where
  fromString = Plain

fromText :: T.Text -> LogMsg
fromText = fromString . T.unpack

instance Semigroup LogMsg where
  Concat ms1 <> Concat ms2 = Concat $ ms1 <> ms2
  Concat ms1 <> ms2 = Concat $ ms1 <> [ms2]
  ms1 <> Concat ms2 = Concat $ ms1 : ms2
  m1 <> m2 = Concat [m1, m2]

instance Monoid LogMsg where
  mempty = Plain ""

styled :: Style -> LogMsg -> LogMsg
styled s m = Styled s m

toPlainString :: LogMsg -> String
toPlainString (Plain m) = m
toPlainString (Styled _ m) = toPlainString m
toPlainString (Concat ms) = concat $ toPlainString <$> ms

toTermString :: LogMsg -> String
toTermString (Plain m) = m
toTermString (Styled s m) =
  Term.applyStyles (styleToTermStyles s) $ toTermString m
  where
    styleToTermStyles = \case
      Important -> [Term.Bold, Term.Magenta]
      Generating -> [Term.Cyan]
      Fixing -> [Term.Green]
      Error -> [Term.Red]
      Custom styles -> styles
toTermString (Concat ms) = concat $ toTermString <$> ms
