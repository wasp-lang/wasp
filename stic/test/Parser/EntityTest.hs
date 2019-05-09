module Parser.EntityTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)

import Parser.Common (runWaspParser)
import Parser.Entity
    ( entityField
    , entityFields
    , entity
    , entityFieldType
    )
import qualified Wasp


spec_parseEntity :: Spec
spec_parseEntity = do
    let titleStringField = Wasp.EntityField "title" Wasp.EftString
    let isDoneBoolField = Wasp.EntityField "isDone" Wasp.EftBoolean

    describe "Parsing entity" $ do
        it "When given a valid entity declaration, parses it correctly" $ do
            runWaspParser entity "entity Task { title :: string, isDone :: boolean }"
                `shouldBe` Right Wasp.Entity 
                { Wasp.entityName = "Task"
                , Wasp.entityFields = [titleStringField, isDoneBoolField]
                }

    describe "Parsing entity fields separated with a comma" $ do
        it "When given multiple comma-separated fields, parses them correctly." $ do
            runWaspParser entityFields "title :: string, isDone :: boolean"
                `shouldBe` Right 
                [ titleStringField
                , isDoneBoolField
                ]

    describe "Parsing entity field declaration" $ do
        it "When given a valid field declaration, parses it correctly" $ do
            runWaspParser entityField "title :: string"
                `shouldBe` Right (Wasp.EntityField "title" Wasp.EftString)

        it "When given an invalid field declaration, returns Left" $ do
            isLeft (runWaspParser entityField "title <>")
                `shouldBe` True

    describe "Parsing entity field type declaration" $ do
        it "When given a field type of string, parses it correctly" $ do
            runWaspParser entityFieldType "string"
                `shouldBe` Right Wasp.EftString

        it "When given a field type of boolean, parses it correctly" $ do
            runWaspParser entityFieldType "boolean"
                `shouldBe` Right Wasp.EftBoolean

        it "When given an unknown field type, returns Left" $ do
            isLeft (runWaspParser entityFieldType "unknownType")
                `shouldBe` True
