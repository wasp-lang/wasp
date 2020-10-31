#!/usr/bin/env stack
{-
  stack script
    --resolver lts-16.14
    --package turtle
    --package regex-tdfa
    --package text
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad      (when)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T.IO
import           System.Environment (getArgs)
import qualified Text.Regex.TDFA    as TR
import           Turtle             (empty, shell)

main = do
    args <- getArgs
    case args of
        [version] -> makeNewRelease version
        _         -> putStrLn "Usage: ./new-release.sh <version>"

makeNewRelease :: String -> IO ()
makeNewRelease newVersion = do
    oldPackageYamlContents <- T.unpack <$> T.IO.readFile "package.yaml"

    (newPackageYamlContents, oldVersion) <- updatePackageYaml oldPackageYamlContents newVersion
    let tag = "v" ++ newVersion

    when (newVersion == oldVersion) $ error "Version you provided is the current version!"

    putStrLn $ unlines
        [ "This will update wasp version from " ++ oldVersion ++ " to " ++ newVersion
        , "package.yaml will be updated, and this change will be commited and pushed."
        , "Also, tag " ++ tag ++ " will be created and pushed, triggering CI to"
          ++ " create new release on Github."
        , "Are you sure you want to proceed (y/n)?"
        ]
    confirmation <- getLine
    when (confirmation /= "y") $ error "Aborting"

    T.IO.writeFile "package.yaml" $ T.pack newPackageYamlContents

    -- TODO: Print what I am doing for each step.

    putStrLn "\nCreating new commit with updated package.yaml and pushing it.\n"
    shell "git add package.yaml" empty
    shell (T.pack $ "git commit -m \"Updated wasp version to " ++ newVersion ++ ".\"") empty
    shell "git push" empty

    putStrLn $ "\nCreating tag " ++ tag ++ " and pushing it.\n"
    shell (T.pack $ "git tag " ++ tag) empty
    shell (T.pack $ "git push origin " ++ tag) empty

    return ()

updatePackageYaml :: String -> String -> IO (String, String) -- Returns (update package yaml contents, old version)
updatePackageYaml packageYamlContents newVersion = do
    let (beforeMatch, match, afterMatch, submatches) =
            packageYamlContents TR.=~ ("(version: *)([^ #]+)( *# *%WASP_VERSION%)" :: String) :: (String, String, String, [String])
    when (null match) $
        error ("Couldn't locate version in package.yaml, make sure it is annotated"
               ++ " with line comment starting with %WASP_VERSION% (in the same line).")
    let [matchBeforeVersion, oldVersion, matchAfterVersion] = submatches
    let newContents = beforeMatch ++ matchBeforeVersion ++ newVersion ++ matchAfterVersion ++ afterMatch
    return (newContents, oldVersion)
