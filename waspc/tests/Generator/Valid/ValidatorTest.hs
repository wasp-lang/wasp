module Generator.Valid.ValidatorTest (spec_Validator) where

import Test.Hspec
import qualified Wasp.Generator.Valid.Validator as V

spec_Validator :: Spec
spec_Validator = do
  describe "execValidator" $ do
    it "returns empty list when validation succeeds" $ do
      V.execValidator (const V.success) () `shouldBe` []

    it "returns list of errors when validation fails" $ do
      let validator _ = V.failure "test error"
      let errors = V.message <$> V.execValidator validator ()
      errors `shouldBe` ["test error"]

    it "returns multiple errors when validation fails multiple times" $ do
      let validator = V.all [const $ V.failure "error 1", const $ V.failure "error 2"]
      let errors = V.message <$> V.execValidator validator ()
      errors `shouldBe` ["error 1", "error 2"]

  describe "success" $ it "creates a successful validation" $ do
    V.execValidator (const V.success) () `shouldBe` []

  describe "failure" $ do
    it "creates a failed validation with error message" $ do
      let errors = V.message <$> V.execValidator (const $ V.failure "test error") ()
      errors `shouldBe` ["test error"]

    it "creates error with empty field path and no file name" $ do
      let errors = V.execValidator (const $ V.failure "test") ()
      let err = head errors
      V.fieldPath err `shouldBe` []
      V.fileName err `shouldBe` Nothing

  describe "withFileName" $ do
    it "adds file name to validation errors" $ do
      let validator = V.withFileName "config.json" (const $ V.failure "test error")
      let errors = V.execValidator validator ()
      V.fileName (head errors) `shouldBe` Just "config.json"

    it "preserves error message" $ do
      let validator = V.withFileName "config.json" (const $ V.failure "test error")
      let errors = V.execValidator validator ()
      V.message (head errors) `shouldBe` "test error"

    it "adds file name to multiple errors" $ do
      let innerValidator = V.all [const $ V.failure "error 1", const $ V.failure "error 2"]
      let validator = V.withFileName "test.json" innerValidator
      let errors = V.execValidator validator ()
      all (\e -> V.fileName e == Just "test.json") errors `shouldBe` True

  describe "inField" $ do
    it "adds field name to error path" $ do
      let validator = V.inField ("username", id) (const $ V.failure "invalid")
      let errors = V.execValidator validator ()
      V.fieldPath (head errors) `shouldBe` ["username"]

    it "applies getter function to extract field" $ do
      let validator = V.inField ("name", fst) $ \name -> if name == "valid" then V.success else V.failure "invalid name"
      V.execValidator validator ("valid" :: String, 123 :: Int) `shouldBe` []
      length (V.execValidator validator ("invalid" :: String, 123 :: Int)) `shouldSatisfy` (> 0)

    it "creates nested field paths" $ do
      let innerValidator = V.inField ("email", id) (const $ V.failure "invalid")
      let outerValidator = V.inField ("user", id) innerValidator
      let errors = V.execValidator outerValidator ()
      V.fieldPath (head errors) `shouldBe` ["user", "email"]

    it "preserves error message" $ do
      let validator = V.inField ("field", id) (const $ V.failure "test error")
      let errors = V.execValidator validator ()
      V.message (head errors) `shouldBe` "test error"

  describe "eqJust" $ do
    it "succeeds when value matches expected" $ do
      let validator = V.eqJust ("expected" :: String)
      V.execValidator validator (Just "expected") `shouldBe` []

    it "fails when value doesn't match expected" $ do
      let validator = V.eqJust ("expected" :: String)
      let errors = V.message <$> V.execValidator validator (Just "actual")
      errors `shouldBe` ["Expected \"expected\" but got \"actual\"."]

    it "fails when value is Nothing" $ do
      let validator = V.eqJust ("expected" :: String)
      let errors = V.message <$> V.execValidator validator Nothing
      errors `shouldBe` ["Missing value, expected \"expected\"."]

  describe "and" $ do
    it "succeeds when both validators succeed" $ do
      let validator = V.and (const V.success) (const V.success)
      V.execValidator validator () `shouldBe` []

    it "returns failure from the second validator when the first succeeds" $ do
      let validator = V.and (const V.success) (const $ V.failure "second failed")
      let errors = V.message <$> V.execValidator validator ()
      errors `shouldBe` ["second failed"]

    it "short-circuits when the first validator fails" $ do
      let validator =
            V.and
              (const $ V.failure "first failed")
              (\_ -> error "Second validator should not run")
      let errors = V.message <$> V.execValidator validator ()
      errors `shouldBe` ["first failed"]

  describe "all" $ do
    it "succeeds when all validators succeed" $ do
      let validators = [const V.success, const V.success, const V.success]
      V.execValidator (V.all validators) () `shouldBe` []

    it "accumulates all errors from failing validators" $ do
      let validators =
            [ const $ V.failure "error 1",
              const V.success,
              const $ V.failure "error 2",
              const $ V.failure "error 3"
            ]
      let errors = V.message <$> V.execValidator (V.all validators) ()
      errors `shouldBe` ["error 1", "error 2", "error 3"]

    it "succeeds with empty list of validators" $ V.execValidator (V.all []) () `shouldBe` []

  describe "combined validation scenarios" $ do
    it "combines withFileName and inField correctly" $ do
      let validator =
            V.withFileName "config.json" $
              V.inField ("database", id) $
                const (V.failure "connection failed")
      let errors = V.execValidator validator ()
      let err = head errors
      V.fileName err `shouldBe` Just "config.json"
      V.fieldPath err `shouldBe` ["database"]
      V.message err `shouldBe` "connection failed"

    it "validates complex nested structure" $ do
      let validator =
            V.withFileName "app.config" $
              V.all
                [ V.inField ("name", fst) $ V.eqJust ("MyApp" :: String),
                  V.inField ("version", snd) $ V.eqJust (1 :: Int)
                ]
      V.execValidator validator (Just ("MyApp" :: String), Just (1 :: Int)) `shouldBe` []
      let errors = V.message <$> V.execValidator validator (Just ("WrongApp" :: String), Just (2 :: Int))
      errors `shouldBe` ["Expected \"MyApp\" but got \"WrongApp\".", "Expected 1 but got 2."]
