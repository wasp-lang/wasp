module Generator.Valid.ValidatorTest (spec_Validator) where

import qualified Data.Map as Map
import Test.Hspec
import qualified Wasp.Generator.Valid.Validator as V
import Prelude hiding (fail)

spec_Validator :: Spec
spec_Validator = do
  describe "execValidator" $ do
    it "returns empty list when validation succeeds" $ do
      V.execValidator (const V.success) () `shouldBe` []

    it "returns list of errors when validation fails" $ do
      V.execValidator (const $ V.failure "test error") ()
        `shouldBe` [ V.ValidationError
                       { V.message = "test error",
                         V.fieldPath = [],
                         V.fileName = Nothing
                       }
                   ]

  describe "Validator constructor helpers" $ do
    specify "success" $ do
      const V.success ~> []

    specify "failure" $ do
      const (V.failure "error") ~> ["error"]

  describe "Validator combinators" $ do
    specify "and" $ do
      V.and succeed succeed ~> []
      V.and (fail "left") succeed ~> ["left"]
      V.and succeed (fail "right") ~> ["right"]
      V.and (fail "left") (fail "right") ~> ["left"]

    specify "all" $ do
      V.all [] ~> []
      V.all [succeed] ~> []
      V.all [succeed, succeed] ~> []
      V.all [fail "error 1"] ~> ["error 1"]
      V.all [fail "error 1", fail "error 2"] ~> ["error 1", "error 2"]
      V.all [succeed, fail "error 1", succeed, fail "error 2"] ~> ["error 1", "error 2"]

    specify "withFileName" $ do
      V.withFileName "config.json" succeed ~> []
      V.withFileName "config.json" (fail "error") ~> ["error"]
      V.withFileName "config.json" (fail "error")
        ~~> ((Just "config.json" ==) . V.fileName)

    specify "inField" $ do
      V.inField ("foo", id) succeed ~> []
      V.inField ("foo", id) (fail "error") ~> ["error"]
      V.inField ("foo", id) (fail "error")
        ~~> ((["foo"] ==) . V.fieldPath)
      V.inField ("foo", id) (V.inField ("bar", id) (fail "error"))
        ~~> ((["foo", "bar"] ==) . V.fieldPath)

    specify "eqJust" $ do
      V.eqJust True <-- Just True ~> []
      V.eqJust True <-- Just False ~> ["Expected True but got False."]
      V.eqJust True <-- Nothing ~> ["Missing value, expected True."]

    specify "combined usage" $ do
      let mockData = Map.fromList [("database", "mysql")] :: Map.Map String String
          validator =
            V.withFileName "config.json" $
              V.all
                [ V.inField ("database", Map.lookup "database") $
                    V.all
                      [ fail "connection failed",
                        fail "timeout occurred",
                        fail "some unknown error" `V.and` fail "some other error",
                        V.eqJust "postgres"
                      ],
                  V.inField ("host", Map.lookup "host") $
                    V.eqJust "localhost"
                ]

      V.execValidator validator mockData
        `shouldBe` [ V.ValidationError
                       { message = "connection failed",
                         fieldPath = ["database"],
                         fileName = Just "config.json"
                       },
                     V.ValidationError
                       { message = "timeout occurred",
                         fieldPath = ["database"],
                         fileName = Just "config.json"
                       },
                     V.ValidationError
                       { message = "some unknown error",
                         fieldPath = ["database"],
                         fileName = Just "config.json"
                       },
                     V.ValidationError
                       { message = "Expected \"postgres\" but got \"mysql\".",
                         fieldPath = ["database"],
                         fileName = Just "config.json"
                       },
                     V.ValidationError
                       { message = "Missing value, expected \"localhost\".",
                         fieldPath = ["host"],
                         fileName = Just "config.json"
                       }
                   ]

succeed :: a -> V.Validation
succeed = const V.success

fail :: String -> a -> V.Validation
fail = const . V.failure

-- | Checks that the validator produces exactly the expected error messages
(~>) :: V.Validator () -> [String] -> Expectation
validator ~> expectedErrorMessages =
  V.message <$> V.execValidator validator () `shouldBe` expectedErrorMessages

-- | Checks that all the validator errors satisfy a given predicate
(~~>) :: V.Validator () -> (V.ValidationError -> Bool) -> Expectation
validator ~~> f =
  mapM_ (`shouldSatisfy` f) (V.execValidator validator ())

-- | Passes the given input to the `Validator`, and turns it into a `Validator ()`.
-- This way we can use our shorthand operators, which only supply `()`.
(<--) :: V.Validator a -> a -> V.Validator ()
validator <-- value = const $ validator value
