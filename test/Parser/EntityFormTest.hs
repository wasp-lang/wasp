module Parser.EntityFormTest where

import Test.Tasty.Hspec

import Parser.Common (runWaspParser)
import Parser.EntityForm
    ( entityForm
    , submitConfig

    , entityFormOptionSubmit
    , EntityFormOption (..)
    )
import qualified Wasp.EntityForm as EF

spec_parseEntityForm :: Spec
spec_parseEntityForm = do

    describe "Parsing Wasp element entity-form" $ do
        it "When given an entity form with submit config, it is included in the result." $ do
            runWaspParser entityForm "entity-form<Task> someEntityForm { submit: {onEnter: true} }"
                `shouldBe` Right EF.EntityForm
                    { EF._name = "someEntityForm"
                    , EF._entityName = "Task"
                    , EF._submit = Just (EF.Submit
                        { EF._onEnter = Just True
                        , EF._submitButton = Nothing
                        }
                      )
                    }

    it "When given an entity form without submit config, it is not included in the result." $ do
        runWaspParser entityForm "entity-form<Task> someEntityForm {}"
            `shouldBe` Right EF.EntityForm
                { EF._name = "someEntityForm"
                , EF._entityName = "Task"
                , EF._submit = Nothing
                }

    describe "Parsing submit option - submit: {...}" $ do
        it "When given a valid submit option declaration, parses it correctly." $ do
            runWaspParser entityFormOptionSubmit "submit: { onEnter: true }"
                `shouldBe` Right (EfoSubmit EF.Submit
                    { EF._onEnter = Just True
                    , EF._submitButton = Nothing
                    })

    describe "Parsing submit config - closure content" $ do
        it "When given a valid submit configuration, parses it correctly." $ do
            runWaspParser submitConfig "onEnter: true"
                `shouldBe` Right EF.Submit
                    { EF._onEnter = Just True
                    , EF._submitButton = Nothing
                    }
