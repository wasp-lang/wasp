module Util.InteractiveTest where

import Data.List (mapAccumL)
import qualified Data.Text as T
import Test.Hspec
import Wasp.Util.Interactive (OutputSource (..), OutputTrail (..), initialOutputTrail, separate)

spec_outputSeparation :: Spec
spec_outputSeparation = do
  it "puts an empty line between Wasp's output and the job logs" $
    written [(WaspMessage, "  Server: http://localhost:3001/\n"), (JobLog, "[server] listening\n")]
      `shouldBe` "  Server: http://localhost:3001/\n\n[server] listening\n"

  it "puts an empty line between the job logs and Wasp's output" $
    written [(JobLog, "[server] GET /\n"), (WaspMessage, "✔ Recompiled the project\n")]
      `shouldBe` "[server] GET /\n\n✔ Recompiled the project\n"

  it "keeps output from one source together" $
    written [(WaspMessage, "✔ Compiled the project\n"), (WaspMessage, "!  WARNING  Something\n")]
      `shouldBe` "✔ Compiled the project\n!  WARNING  Something\n"

  it "does not add to an empty line that is already there" $
    written [(JobLog, "[server] GET /\n"), (WaspMessage, "\n"), (JobLog, "[server] GET /\n")]
      `shouldBe` "[server] GET /\n\n[server] GET /\n"

  it "lets an explicit empty line stand in for the separation" $
    written [(JobLog, "[server] GET /\n"), (WaspMessage, "\n"), (WaspMessage, "✔ Recompiled the project\n")]
      `shouldBe` "[server] GET /\n\n✔ Recompiled the project\n"

  it "starts the very first output without an empty line" $
    written [(JobLog, "[server] listening\n")]
      `shouldBe` "[server] listening\n"

  it "starts Wasp's next output on a new block after the block ended, letting the job logs go on" $
    writtenFrom
      afterBlockEnd
      [(JobLog, "[server] GET /\n"), (JobLog, "[server] GET /\n"), (WaspMessage, "✔ Recompiled the project\n")]
      `shouldBe` "\n[server] GET /\n[server] GET /\n\n✔ Recompiled the project\n"

  it "separates two blocks of Wasp's output when the block ended in between" $
    writtenFrom
      afterBlockEnd
      [(WaspMessage, "✔ Recompiled the project\n"), (WaspMessage, "!  WARNING  Something\n")]
      `shouldBe` "\n✔ Recompiled the project\n!  WARNING  Something\n"

written :: [(OutputSource, T.Text)] -> T.Text
written = writtenFrom initialOutputTrail

writtenFrom :: OutputTrail -> [(OutputSource, T.Text)] -> T.Text
writtenFrom trail = T.concat . snd . mapAccumL (uncurry . separate) trail

afterBlockEnd :: OutputTrail
afterBlockEnd = OutputTrail (Just WaspMessage) 1 True
