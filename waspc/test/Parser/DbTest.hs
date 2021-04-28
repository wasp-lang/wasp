module Parser.DbTest where

import Data.Either (isLeft)
import Parser.Common (runWaspParser)
import Parser.Db (db)
import Test.Tasty.Hspec
import qualified Wasp.Db

spec_parseDb :: Spec
spec_parseDb =
  describe "Parsing db declaration" $ do
    let parseDb input = runWaspParser db input

    it "When given a valid db declaration, returns correct AST" $ do
      parseDb "db { system: PostgreSQL }"
        `shouldBe` Right (Wasp.Db.Db {Wasp.Db._system = Wasp.Db.PostgreSQL})
      parseDb "db { system: SQLite }"
        `shouldBe` Right (Wasp.Db.Db {Wasp.Db._system = Wasp.Db.SQLite})

    it "When given db wasp declaration without 'db', should return Left" $ do
      isLeft (parseDb "db { }") `shouldBe` True
