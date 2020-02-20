module WaspTest where

import Test.Tasty.Hspec

import Wasp
import qualified Fixtures as F

spec_getEntityFormsForEntity :: Spec
spec_getEntityFormsForEntity = do

    let waspWithEntityForm = fromWaspElems
            [ WaspElementEntity F.taskEntity
            , WaspElementEntityForm F.taskCreateForm
            ]

    let waspWithoutEntityForm = fromWaspElems
            [ WaspElementEntity F.taskEntity
            ]

    it "When given Wasp record which contains an entity-form for a given entity, returns it." $ do
        getEntityFormsForEntity waspWithEntityForm F.taskEntity
            `shouldBe` [F.taskCreateForm]

    it "When given Wasp record without an entity-form for a given entity, returns Nothing." $ do
        getEntityFormsForEntity waspWithoutEntityForm F.taskEntity `shouldBe` []
