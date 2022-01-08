module Analyzer.Evaluation.EvaluationErrorTest where

import Analyzer.TestUtil (ctx)
import Test.Tasty.Hspec
import Wasp.Analyzer.Evaluator.EvaluationError
import Wasp.Analyzer.Type (Type (..))

spec_EvaluationError :: Spec
spec_EvaluationError = do
  describe "Analyzer.Evaluator.EvaluationError" $ do
    describe "getErrorMessageAndCtx works correctly for" $ do
      it "InvalidEnumVariant error" $ do
        let ctx1 = ctx (1, 1) (1, 10)
        let err = mkEvaluationError ctx1 $ InvalidEnumVariant "Animal" ["Cow", "Dog"] "Car"
        getErrorMessageAndCtx err
          `shouldBe` ( "Expected value of enum type 'Animal' but got value 'Car'"
                         ++ "\nValid values: Cow | Dog",
                       ctx1
                     )
      it "ExpectedType error" $ do
        let ctx1 = ctx (1, 4) (2, 5)
        let err = mkEvaluationError ctx1 $ ExpectedType NumberType StringType
        getErrorMessageAndCtx err `shouldBe` ("Expected type: number\nActual type:   string", ctx1)
      it "ExpectedTupleType error" $ do
        let ctx1 = ctx (1, 4) (2, 5)
        let err = mkEvaluationError ctx1 $ ExpectedTupleType 3 StringType
        getErrorMessageAndCtx err `shouldBe` ("Expected a tuple of size 3.\nActual type: string", ctx1)
