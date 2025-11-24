module Generator.Valid.ValidatorTest (spec_Validator) where

import Test.Hspec
import qualified Wasp.Generator.Valid.Validator as V

spec_Validator :: Spec
spec_Validator = do
  describe "execValidator" $ do
    it "returns empty list when validation succeeds" $ do
      let validator = const V.success

      testSuccess
        validator
        ()

    it "returns list of errors when validation fails" $ do
      let validator _ = V.failure "test error"

      testFailure
        validator
        ()
        ["test error"]

    it "returns multiple errors when validation fails multiple times" $ do
      let validator = V.all [const $ V.failure "error 1", const $ V.failure "error 2"]

      testFailure
        validator
        ()
        ["error 1", "error 2"]

  describe "success" $ it "creates a successful validation" $ do
    let validator = const V.success

    testSuccess
      validator
      ()

  describe "failure" $ do
    it "creates a failed validation with error message" $ do
      let validator = const $ V.failure "test error"

      testFailure
        validator
        ()
        ["test error"]

    it "creates error with empty field path and no file name" $ do
      let validator = const $ V.failure "test"

      testFieldPath
        validator
        ()
        []

      testFileName
        validator
        ()
        Nothing

  describe "withFileName" $ do
    it "adds file name to validation errors" $ do
      let innerValidator = const $ V.failure "test error"
      let validator = V.withFileName "config.json" innerValidator

      testFileName
        validator
        ()
        (Just "config.json")

    it "preserves error message" $ do
      let innerValidator = const $ V.failure "test error"
      let validator = V.withFileName "config.json" innerValidator

      testFailure
        validator
        ()
        ["test error"]

    it "adds file name to multiple errors" $ do
      let innerValidator = V.all [const $ V.failure "error 1", const $ V.failure "error 2"]
      let validator = V.withFileName "test.json" innerValidator

      testFileName
        validator
        ()
        (Just "test.json")

  describe "inField" $ do
    it "adds field name to error path" $ do
      let innerValidator = const $ V.failure "invalid"
      let validator = V.inField ("username", id) innerValidator

      testFieldPath
        validator
        ()
        ["username"]

    it "applies getter function to extract field" $ do
      let innerValidator = \case
            "valid" -> V.success
            _ -> V.failure "invalid name"
      let validator = V.inField ("name", fst) innerValidator

      do
        let value = ("valid", 123) :: (String, Int)
        testSuccess
          validator
          value

      do
        let value = ("invalid", 123) :: (String, Int)
        testFailure
          validator
          value
          ["invalid name"]

    it "creates nested field paths" $ do
      let innerValidator = const $ V.failure "invalid"
      let innerValidator' = V.inField ("email", id) innerValidator
      let validator = V.inField ("user", id) innerValidator'

      testFieldPath
        validator
        ()
        ["user", "email"]

    it "preserves error message" $ do
      let innerValidator = const $ V.failure "test error"
      let validator = V.inField ("field", id) innerValidator

      testFailure
        validator
        ()
        ["test error"]

  describe "eqJust" $ do
    it "succeeds when value matches expected" $ do
      let validator = V.eqJust ("expected" :: String)

      testSuccess
        validator
        (Just "expected")

    it "fails when value doesn't match expected" $ do
      let validator = V.eqJust ("expected" :: String)

      testFailure
        validator
        (Just "actual")
        ["Expected \"expected\" but got \"actual\"."]

    it "fails when value is Nothing" $ do
      let validator = V.eqJust ("expected" :: String)

      testFailure
        validator
        Nothing
        ["Missing value, expected \"expected\"."]

  describe "and" $ do
    it "succeeds when both validators succeed" $ do
      let validator =
            V.and (const V.success) (const V.success)

      testSuccess
        validator
        ()

    it "returns failure from the second validator when the first succeeds" $ do
      let validator =
            V.and (const V.success) (const $ V.failure "second failed")

      testFailure
        validator
        ()
        ["second failed"]

    it "short-circuits when the first validator fails" $ do
      let validator =
            V.and
              (const $ V.failure "first failed")
              (const $ error "Second validator should not run")

      testFailure
        validator
        ()
        ["first failed"]

  describe "all" $ do
    it "succeeds when all validators succeed" $ do
      let validators = [const V.success, const V.success, const V.success]

      testSuccess
        (V.all validators)
        ()

    it "accumulates all errors from failing validators" $ do
      let validators =
            [ const $ V.failure "error 1",
              const V.success,
              const $ V.failure "error 2",
              const $ V.failure "error 3"
            ]

      testFailure
        (V.all validators)
        ()
        ["error 1", "error 2", "error 3"]

    it "succeeds with empty list of validators" $ do
      testSuccess
        (V.all [])
        ()

  describe "combined validation scenarios" $ do
    it "combines withFileName and inField correctly" $ do
      let validator =
            V.withFileName "config.json" $
              V.inField ("database", id) $
                const (V.failure "connection failed")

      testFileName
        validator
        ()
        (Just "config.json")

      testFieldPath
        validator
        ()
        ["database"]

      testFailure
        validator
        ()
        ["connection failed"]

    it "validates complex nested structure" $ do
      let validator =
            V.withFileName "app.config" $
              V.all
                [ V.inField ("name", fst) $ V.eqJust ("MyApp" :: String),
                  V.inField ("version", snd) $ V.eqJust (1 :: Int)
                ]

      do
        let value = (Just "MyApp", Just 1) :: (Maybe String, Maybe Int)

        testSuccess
          validator
          value

      do
        let value = (Just "WrongApp", Just 2) :: (Maybe String, Maybe Int)

        testFailure
          validator
          value
          ["Expected \"MyApp\" but got \"WrongApp\".", "Expected 1 but got 2."]

testSuccess :: V.Validator a -> a -> IO ()
testSuccess validator value =
  V.execValidator validator value `shouldBe` []

testFailure :: V.Validator a -> a -> [String] -> IO ()
testFailure validator value expectedErrors = do
  let errors = V.execValidator validator value
  (V.message <$> errors) `shouldBe` expectedErrors

testFieldPath :: V.Validator a -> a -> [String] -> IO ()
testFieldPath validator value expectedPath = do
  let errors = V.execValidator validator value

  mapM_
    (`shouldBe` expectedPath)
    (V.fieldPath <$> errors)

testFileName :: V.Validator a -> a -> Maybe String -> IO ()
testFileName validator value expectedFileName = do
  let errors = V.execValidator validator value

  mapM_
    (`shouldBe` expectedFileName)
    (V.fileName <$> errors)
