module Analyzer.Evaluation.EvaluationErrorTest where

import Analyzer.TestUtil (ctx)
import Data.List (intercalate)
import Test.Tasty.Hspec
import Wasp.Analyzer.Evaluator.EvaluationError
import Wasp.Analyzer.Parser.Ctx
import Wasp.Analyzer.Type (Type (..))

ctx1 :: Ctx
ctx1 = ctx (1, 4) (2, 5)

nestEvalErrors :: EvaluationError -> [EvalErrorCtx] -> EvaluationError
nestEvalErrors bottomErr wrapperErrs = foldr wrap bottomErr wrapperErrs
  where
    wrap evalErrorCtx = mkEvaluationError ctx1 . WithEvalErrorCtx evalErrorCtx

spec_EvaluationError :: Spec
spec_EvaluationError = do
  describe "Analyzer.Evaluator.EvaluationError" $ do
    describe "getErrorMessageAndCtx works correctly for" $ do
      let expectedTypeError = mkEvaluationError ctx1 $ ExpectedType NumberType StringType
      let expectedTypeErrorMsg = "Expected type: number\nActual type:   string"

      it "InvalidEnumVariant error" $ do
        let err = mkEvaluationError ctx1 $ InvalidEnumVariant "Animal" ["Cow", "Dog"] "Car"
        getErrorMessageAndCtx err
          `shouldBe` ( "Expected value of enum type 'Animal' but got value 'Car'"
                         ++ "\nValid values: Cow | Dog",
                       ctx1
                     )

      it "ExpectedType error" $ do
        getErrorMessageAndCtx expectedTypeError `shouldBe` (expectedTypeErrorMsg, ctx1)

      it "ExpectedTupleType error" $ do
        let expectedTupleTypeError = mkEvaluationError ctx1 $ ExpectedTupleType 3 StringType
        let expectedTupleTypeErrorMsg = "Expected a tuple of size 3.\nActual type: string"
        getErrorMessageAndCtx expectedTupleTypeError `shouldBe` (expectedTupleTypeErrorMsg, ctx1)

      it "ExpectedType error nested in WithEvalContextError" $ do
        let err = nestEvalErrors expectedTypeError [InField "key"]
        let (actualMessage, actualCtx) = getErrorMessageAndCtx err
        actualCtx `shouldBe` ctx1
        actualMessage
          `shouldBe` intercalate
            "\n"
            [ "Expected type: number",
              "Actual type:   string",
              "",
              "-> For dictionary field 'key'"
            ]

      it "ExpectedType error nested in two levels of WithEvalContextError" $ do
        let err = nestEvalErrors expectedTypeError [InTuple, InField "key"]
        let (actualMessage, actualCtx) = getErrorMessageAndCtx err
        actualCtx `shouldBe` ctx1
        actualMessage
          `shouldBe` intercalate
            "\n"
            [ expectedTypeErrorMsg,
              "",
              "-> In tuple:",
              "  -> For dictionary field 'key'"
            ]

      it "ExpectedType error nested in many levels of WithEvalContextError" $ do
        let err = nestEvalErrors expectedTypeError [InList, ForVariable "var", InTuple, InField "key"]
        let (actualMessage, actualCtx) = getErrorMessageAndCtx err
        actualCtx `shouldBe` ctx1
        actualMessage
          `shouldBe` intercalate
            "\n"
            [ expectedTypeErrorMsg,
              "",
              "-> In list:",
              "  -> For variable 'var':",
              "    -> In tuple:",
              "      -> For dictionary field 'key'"
            ]
