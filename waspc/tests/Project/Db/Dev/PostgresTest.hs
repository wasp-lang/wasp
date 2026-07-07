module Project.Db.Dev.PostgresTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.Project.Db.Dev.Postgres (parseDevDbPort)

spec_parseDevDbPort :: Spec
spec_parseDevDbPort = do
  describe "parseDevDbPort" $ do
    it "parses a valid port number" $
      parseDevDbPort "8080" `shouldBe` Right 8080

    it "accepts the lowest valid port (1)" $
      parseDevDbPort "1" `shouldBe` Right 1

    it "accepts the highest valid port (65535)" $
      parseDevDbPort "65535" `shouldBe` Right 65535

    it "rejects 0 as out of range" $
      parseDevDbPort "0" `shouldSatisfy` isLeft

    it "rejects a port above 65535 as out of range" $
      parseDevDbPort "65536" `shouldSatisfy` isLeft

    it "rejects a negative port" $
      parseDevDbPort "-1" `shouldSatisfy` isLeft

    it "rejects non-numeric input" $
      parseDevDbPort "abc" `shouldSatisfy` isLeft

    it "rejects an empty string" $
      parseDevDbPort "" `shouldSatisfy` isLeft
