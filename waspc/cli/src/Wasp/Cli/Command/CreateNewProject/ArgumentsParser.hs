module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    newProjectArgsParser,
  )
where

import Data.List (intercalate)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.Parser (ArgsParser (..))
import Wasp.Project.PackageManager (getPackageManagerName, supportedPackageManagers)

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateName :: Maybe String,
    _packageManagerName :: Maybe String
  }

newProjectArgsParser :: ArgsParser NewProjectArgs
newProjectArgsParser =
  ArgsParser "wasp new" $
    NewProjectArgs
      <$> Opt.optional projectNameParser
      <*> Opt.optional templateNameParser
      <*> Opt.optional packageManagerNameParser
  where
    projectNameParser :: Opt.Parser String
    projectNameParser =
      Opt.strArgument $
        Opt.metavar "PROJECT_NAME"
          <> Opt.help "Name of the new project. Required when running non-interactively"

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "Template to use for the new project"

    packageManagerNameParser :: Opt.Parser String
    packageManagerNameParser =
      Opt.strOption $
        Opt.long "package-manager"
          <> Opt.metavar "PACKAGE_MANAGER"
          <> Opt.help ("Package manager of the new project: " ++ intercalate ", " (getPackageManagerName <$> supportedPackageManagers))
