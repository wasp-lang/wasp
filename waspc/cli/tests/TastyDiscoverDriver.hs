{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --modules=*Test.hs #-}

-- -optF --modules=*Test.hs tells tasty-discover to pick up only files that match *Test.hs.
--   I did not do this for any strong reason so we can remove it in the future if we figure out
--   it is too restrictive.
