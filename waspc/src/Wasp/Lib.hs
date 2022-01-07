module Wasp.Lib
  ( compile,
    Generator.setup,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
  )
where

import Data.List (find, isSuffixOf)
import StrongPath (Abs, Dir, File', Path', relfile)
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.Parser.Ctx (getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (..))
import qualified Wasp.AppSpec as AS
import Wasp.Common (DbMigrationsDir, WaspProjectDir, dbMigrationsDirInWaspProjectDir)
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ExternalCode as ExternalCode
import qualified Wasp.Generator as Generator
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Util (indent)
import qualified Wasp.Util.IO as Util.IO

type CompileError = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO (Either CompileError ())
compile waspDir outDir options = do
  maybeWaspFilePath <- findWaspFile waspDir
  case maybeWaspFilePath of
    Nothing -> return $ Left "Couldn't find a single *.wasp file."
    Just waspFilePath -> do
      waspFileContent <- readFile (SP.fromAbsFile waspFilePath)

      case Analyzer.analyze waspFileContent of
        Left analyzeError -> return $ Left $ showAnalyzeError waspFilePath waspFileContent analyzeError
        Right decls -> do
          externalCodeFiles <- ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
          maybeDotEnvFile <- findDotEnvFile waspDir
          maybeMigrationsDir <- findMigrationsDir waspDir
          let appSpec =
                AS.AppSpec
                  { AS.decls = decls,
                    AS.externalCodeFiles = externalCodeFiles,
                    AS.externalCodeDirPath = CompileOptions.externalCodeDirPath options,
                    AS.migrationDir = maybeMigrationsDir,
                    AS.dotEnvFile = maybeDotEnvFile,
                    AS.isBuild = CompileOptions.isBuild options
                  }
          Right <$> Generator.writeWebAppCode appSpec outDir

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ (waspDir SP.</>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp" `isSuffixOf` SP.toFilePath path
        && (length (SP.toFilePath path) > length (".wasp" :: String))

findDotEnvFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvFile waspDir = do
  let dotEnvAbsPath = waspDir SP.</> [relfile|.env|]
  dotEnvExists <- doesFileExist (SP.toFilePath dotEnvAbsPath)
  return $ if dotEnvExists then Just dotEnvAbsPath else Nothing

findMigrationsDir :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (Dir DbMigrationsDir)))
findMigrationsDir waspDir = do
  let migrationsAbsPath = waspDir SP.</> dbMigrationsDirInWaspProjectDir
  migrationsExists <- doesDirectoryExist $ SP.fromAbsDir migrationsAbsPath
  return $ if migrationsExists then Just migrationsAbsPath else Nothing

-- TODO: Consider extracting and polishing this a bit.
-- TODO: Test
showAnalyzeError :: Path' Abs File' -> String -> Analyzer.AnalyzeError -> String
showAnalyzeError waspFilePath waspFileContent err =
  let (msg, ctx) = Analyzer.getErrorMessageAndCtx err
      srcRegion = getCtxRgn ctx
   in unlines
        [ SP.fromAbsFile waspFilePath ++ " @ " ++ showRgn srcRegion,
          indent 2 msg,
          "",
          indent 2 $ unlines $ getEnumedSrcLinesOfRgn srcRegion
        ]
  where
    showPos (SourcePosition l c) = show l ++ ":" ++ show c
    showRgn (SourceRegion startPos endPos) =
      if startPos == endPos
        then showPos startPos
        else showPos startPos ++ " - " ++ showPos endPos

    -- TODO: Add arrow on top that shows start column, and arrow at the bottom that shows end column.
    getEnumedSrcLinesOfRgn :: SourceRegion -> [String]
    getEnumedSrcLinesOfRgn (SourceRegion (SourcePosition startLineIdx _) (SourcePosition endLineIdx _)) =
      let srcLines = take (endLineIdx - startLineIdx + 1) $ drop startLineIdx (lines waspFileContent)
          enumedSrcLines = zip [startLineIdx ..] srcLines
       in map (\(lineIdx, line) -> prefixWithSpacesTillLength 6 (show lineIdx) ++ " | " ++ line) enumedSrcLines

    prefixWithSpacesTillLength :: Int -> String -> String
    prefixWithSpacesTillLength n str =
      let padded = replicate n ' ' ++ str
       in drop (length padded - n) padded
