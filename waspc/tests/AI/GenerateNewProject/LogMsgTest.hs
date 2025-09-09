module AI.GenerateNewProject.LogMsgTest where

import Test.Tasty.Hspec
import Wasp.AI.GenerateNewProject.LogMsg
import qualified Wasp.Util.Terminal as Term

spec_Wasp_AI_GenerateNewProject_LogMsg :: Spec
spec_Wasp_AI_GenerateNewProject_LogMsg = do
  it "Concatenation" $ do
    toPlainString ("foo" <> "bar") `shouldBe` toPlainString "foobar"
    toPlainString ("foo" <> "bar") <> "buzz" `shouldBe` toPlainString "foobarbuzz"
    toPlainString "foo  " <> (" bar" <> " buzz") `shouldBe` toPlainString "foo   bar buzz"

  it "Styling" $ do
    let styledMsg = styled Important "foo" <> " bar" <> styled Error "buzz"
    toPlainString styledMsg `shouldBe` "foo barbuzz"
    toTermString styledMsg
      `shouldBe` ( Term.applyStyles [Term.Bold, Term.Magenta] "foo"
                     <> " bar"
                     <> Term.applyStyles [Term.Red] "buzz"
                 )
