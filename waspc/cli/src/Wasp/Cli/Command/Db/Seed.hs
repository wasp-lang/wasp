module Wasp.Cli.Command.Db.Seed
  ( seed,
  )
where

import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath ((</>))
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.ExtImport as AS
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import Wasp.Generator.DbGenerator.Operations (dbSeed)
import qualified Wasp.Message as Msg

seed :: Maybe String -> Command ()
seed maybeUserProvidedSeedName = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let genProjectDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  appSpec <- analyze waspProjectDir

  nameOfSeedToRun <- figureOutNameOfExistingSeedToRun maybeUserProvidedSeedName appSpec

  cliSendMessageC $ Msg.Start "Seeding the database..."

  liftIO (dbSeed genProjectDir nameOfSeedToRun) >>= \case
    Left errorMsg -> cliSendMessageC $ Msg.Failure "Database seeding failed" errorMsg
    Right () -> cliSendMessageC $ Msg.Success "Database seeded successfully!"

getDbSeeds :: AS.AppSpec -> Maybe [AS.ExtImport]
getDbSeeds spec = AS.Db.seeds =<< AS.App.db (snd $ ASV.getApp spec)

figureOutNameOfExistingSeedToRun :: Maybe String -> AS.AppSpec -> Command String
figureOutNameOfExistingSeedToRun maybeUserProvidedSeedName spec = case getDbSeeds spec of
  Just seeds@(_ : _) -> do
    let seedNames = map AS.ExtImport.importIdentifier seeds
    case maybeUserProvidedSeedName of
      Just name -> validateUserProvidedSeedName name seedNames
      Nothing -> case seedNames of
        [seedName] -> return seedName
        _seedNames -> liftIO $ askUserToChooseFromSeedNames seedNames
  _noSeeds ->
    (E.throwError . CommandError "No seeds defined") $
      "You haven't defined any database seeding functions, so there is nothing to run!\n"
        <> "To do so, define seeding functions via app.db.seeds in your Wasp config."
  where
    askUserToChooseFromSeedNames :: [String] -> IO String
    askUserToChooseFromSeedNames seedNames = do
      putStrLn "Choose a seed to run:"
      mapM_ (\(i, n) -> putStrLn $ printf "  %d: %s" i n) $ zip [1 :: Int ..] seedNames
      putStrLn "Type a number (e.g. 1 or 2):"
      chosenNumber <- getLine
      case parseNumberInRange (1, length seedNames) chosenNumber of
        Right idx -> return $ seedNames !! (idx - 1)
        Left errMsg -> do
          putStrLn $ "Invalid number (" <> errMsg <> "), please try again."
          askUserToChooseFromSeedNames seedNames

    -- TODO: Move to Wasp.Util?
    parseNumberInRange :: (Int, Int) -> String -> Either String Int
    parseNumberInRange (minNum, maxNum) strNum =
      case reads strNum of
        [(num, _)] ->
          if num >= minNum && num <= maxNum
            then Right num
            else Left "Number out of range."
        _notANum -> Left "Not a number."

    validateUserProvidedSeedName :: String -> [String] -> Command String
    validateUserProvidedSeedName userProvidedSeedName seedNames =
      if userProvidedSeedName `elem` seedNames
        then return userProvidedSeedName
        else
          (E.throwError . CommandError "Invalid seed name") $
            "There is no seed with the name: " <> userProvidedSeedName <> " ."
              <> ("\nValid seed names are: " <> intercalate ", " seedNames <> ".")
