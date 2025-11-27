module Generator.Valid.ValidatorTest (spec_Validator) where

import Test.Hspec
import qualified Wasp.Generator.Valid.Validator as V

spec_Validator :: Spec
spec_Validator = do
  describe "execValidator" $ do
    it "returns empty list when validation succeeds" $ do
      testSuccess alwaysSucceed ()

    it "returns list of errors when validation fails" $ do
      testFailure (alwaysFail "test error") () ["test error"]

  describe "success" $
    it "creates a successful validation" $ do
      testSuccess (const V.success) ()

  describe "failure" $ do
    it "creates a failed validation with error message" $ do
      testFailure (const $ V.failure "test error") () ["test error"]

    it "creates error with empty field path and no file name" $ do
      testErrorsHaveCorrectFieldPath (alwaysFail "test error") () []
      testErrorsHaveCorrectFileName (alwaysFail "test error") () Nothing

  describe "withFileName" $ do
    it "adds file name to validation errors" $ do
      testErrorsHaveCorrectFileName
        (V.withFileName "config.json" $ alwaysFail "test error")
        ()
        (Just "config.json")

    it "preserves error message" $ do
      testFailure
        (V.withFileName "config.json" $ alwaysFail "test error")
        ()
        ["test error"]

    it "adds file name to multiple errors" $ do
      testErrorsHaveCorrectFileName
        (V.withFileName "test.json" $ V.all [alwaysFail "error 1", alwaysFail "error 2"])
        ()
        (Just "test.json")

  describe "inField" $ do
    -- Test data for nested structure validation
    let mockStructure =
          DummyStructure
            { foo = "foo",
              bar = DummySubstructure {subfoo = 42, subbar = True}
            }

    it "correctly gets the inner values" $ do
      let validator = V.inField ("foo", foo) $ \case
            "foo" -> V.success
            _ -> V.failure "test error"
      testSuccess validator mockStructure
      testFailure validator (mockStructure {foo = "bar"}) ["test error"]

    it "preserves error message" $ do
      testFailure
        (V.inField ("foo", foo) $ alwaysFail "test error")
        mockStructure
        ["test error"]

    it "sets field paths in the validation errors" $ do
      testErrorsHaveCorrectFieldPath
        ( V.inField ("bar", bar) $
            V.inField ("subfoo", subfoo) $
              alwaysFail "test error"
        )
        mockStructure
        ["bar", "subfoo"]

  describe "eqJust" $ do
    it "succeeds when value matches expected" $ do
      let validator = V.eqJust ("expected" :: String)
      testSuccess validator (Just "expected")

    it "fails when value doesn't match expected" $ do
      let validator = V.eqJust ("expected" :: String)
      testFailure validator (Just "actual") ["Expected \"expected\" but got \"actual\"."]

    it "fails when value is Nothing" $ do
      let validator = V.eqJust ("expected" :: String)
      testFailure validator Nothing ["Missing value, expected \"expected\"."]

  describe "validatorand" $ do
    it "valid and valid -> valid" $ do
      testSuccess (alwaysSucceed `V.and` alwaysSucceed) ()

    it "invalid1 and valid -> invalid1" $ do
      testFailure (alwaysFail "left" `V.and` alwaysSucceed) () ["left"]

    it "valid and invalid2 -> invalid2" $ do
      testFailure (alwaysSucceed `V.and` alwaysFail "right") () ["right"]

    it "invalid1 and invalid2 -> invalid1" $ do
      testFailure (alwaysFail "left" `V.and` alwaysFail "right") () ["left"]

  describe "all" $ do
    it "succeeds when all validators succeed" $ do
      let validators = [alwaysSucceed, alwaysSucceed, alwaysSucceed]
      testSuccess (V.all validators) ()

    it "accumulates errors from failing validators" $ do
      let validators =
            [ alwaysFail "error 1",
              alwaysSucceed,
              alwaysFail "error 2",
              alwaysFail "error 3"
            ]
      testFailure (V.all validators) () ["error 1", "error 2", "error 3"]

    it "succeeds with empty list of validators" $ do
      testSuccess (V.all []) ()

  describe "combined validation scenarios" $ do
    it "combines withFileName and inField correctly" $ do
      let validator =
            V.withFileName "config.json" $
              V.inField ("database", id) $
                const (V.failure "connection failed")

      testErrorsHaveCorrectFileName validator () (Just "config.json")
      testErrorsHaveCorrectFieldPath validator () ["database"]
      testFailure validator () ["connection failed"]

alwaysSucceed :: b -> V.Validation
alwaysSucceed = const V.success

alwaysFail :: String -> b -> V.Validation
alwaysFail msg = const $ V.failure msg

testSuccess :: V.Validator a -> a -> IO ()
testSuccess validator value =
  V.execValidator validator value `shouldBe` []

testFailure :: V.Validator a -> a -> [String] -> IO ()
testFailure validator value expectedErrors = do
  let errors = V.execValidator validator value
  V.message <$> errors `shouldBe` expectedErrors

testErrorsHaveCorrectFieldPath :: V.Validator a -> a -> [String] -> IO ()
testErrorsHaveCorrectFieldPath validator value expectedPath = do
  let errors = V.execValidator validator value
  mapM_ (`shouldBe` expectedPath) (V.fieldPath <$> errors)

testErrorsHaveCorrectFileName :: V.Validator a -> a -> Maybe String -> IO ()
testErrorsHaveCorrectFileName validator value expectedFileName = do
  let errors = V.execValidator validator value
  mapM_ (`shouldBe` expectedFileName) (V.fileName <$> errors)

-- These are dummy structures for use when testing inField validator
data DummyStructure = DummyStructure {foo :: String, bar :: DummySubstructure}

data DummySubstructure = DummySubstructure {subfoo :: Int, subbar :: Bool}
