module Wasp.Util.TH
  ( quasiQuoterFromParser,
  )
where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (lift))

quasiQuoterFromParser :: (Show a, Lift t) => (String -> Either a t) -> QuasiQuoter
quasiQuoterFromParser parse =
  QuasiQuoter
    { quoteExp = either (fail . show) lift . parse,
      quotePat = err "pattern",
      quoteType = err "type",
      quoteDec = err "declaration"
    }
  where
    err what x = fail ("unexpected " ++ what ++ ", must be expression: " ++ x)
