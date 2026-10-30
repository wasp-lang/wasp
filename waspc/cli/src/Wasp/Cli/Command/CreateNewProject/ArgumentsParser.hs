module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    newProjectArgsParser,
  )
where

import Control.Applicative ((<|>))
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.Parser (ArgsParser (..))

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateName :: Maybe String,
    -- | Nothing means the user didn't say, so we ask (or install, when not interactive).
    _shouldInstallDeps :: Maybe Bool
  }

newProjectArgsParser :: ArgsParser NewProjectArgs
newProjectArgsParser =
  ArgsParser "wasp new" $
    NewProjectArgs
      <$> Opt.optional projectNameParser
      <*> Opt.optional templateNameParser
      <*> Opt.optional shouldInstallDepsParser
  where
    projectNameParser :: Opt.Parser String
    projectNameParser = Opt.strArgument $ Opt.metavar "PROJECT_NAME"

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "Template to use for the new project"

    shouldInstallDepsParser :: Opt.Parser Bool
    shouldInstallDepsParser =
      Opt.flag'
        True
        ( Opt.long "install"
            <> Opt.help "Install the npm dependencies after creating the project (the default when not asked)"
        )
        <|> Opt.flag'
          False
          ( Opt.long "no-install"
              <> Opt.help "Don't install the npm dependencies after creating the project"
          )
