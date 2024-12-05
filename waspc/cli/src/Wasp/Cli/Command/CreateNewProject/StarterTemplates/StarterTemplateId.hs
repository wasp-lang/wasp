module Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplateId
  ( getStarterTemplateByIdOrThrow,
    findTemplateByName,
  )
where

import Control.Arrow (left)
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import StrongPath (Dir', Path', Rel', parseRelDir, reldir)
import qualified System.FilePath.Posix as FP.Posix
import qualified Text.Parsec as P
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwInvalidTemplateNameUsedError)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common (styleText, waspVersionTemplateGitTag)
import qualified Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplate as ST
import qualified Wasp.Cli.GithubRepo as GhRepo

data StarterTemplateId
  = GhRepoTemplateUri !GhRepo.GithubRepoOwner !GhRepo.GithubRepoName !(Maybe (Path' Rel' Dir'))
  | EmbeddedTemplateName !String

-- TODO: Comment (explain it will try to find it in featured templates or construct it based on id).
getStarterTemplateByIdOrThrow :: [ST.StarterTemplate] -> String -> Command ST.StarterTemplate
getStarterTemplateByIdOrThrow availableTemplates templateId =
  -- TODO: Refactor/rename throwInvalidTemplateNameUsedError? Yeah or probably just make another error function here, that is better suited for the situation (which is failed parsing of the template id.
  (parseStarterTemplateId templateId & either (const throwInvalidTemplateNameUsedError) pure) >>= \case
    EmbeddedTemplateName templateName ->
      findTemplateByName availableTemplates templateName
        & maybe throwInvalidTemplateNameUsedError pure
    GhRepoTemplateUri repoOwner repoName maybeTmplDirPath ->
      return $
        ST.GhRepoStarterTemplate
          (GhRepo.GithubRepoRef repoOwner repoName waspVersionTemplateGitTag)
          ( ST.DirBasedTemplateMetadata
              { ST._name = repoName,
                ST._description = "Template from Github repo " <> repoOwner <> "/" <> repoName,
                ST._path = maybeTmplDirPath & fromMaybe [reldir|.|],
                ST._buildStartingInstructions = \projectDirName ->
                  unlines
                    -- TODO: Improve next line, repoName in projectDirName doesn't make much sense.
                    [ styleText $ "Created new project from template " <> repoName <> " in " <> projectDirName <> " !",
                      styleText $ "Check github.com/" <> repoOwner <> "/" <> repoName <> " for starting instructions."
                    ]
              }
          )

-- TODO: Write a comment here.
parseStarterTemplateId :: String -> Either String StarterTemplateId
parseStarterTemplateId = \case
  templateId
    | ghRepoTemplateIdPrefix `isPrefixOf` templateId ->
        -- TODO: Do something with the parse error message or just let it through as I do now?
        parseGhRepoTemplateUri templateId & left show
  templateId ->
    pure $ EmbeddedTemplateName templateId
  where
    -- Parses following format: github:<org_name>/<repo_name>[/<path_to_template_dir>] .
    parseGhRepoTemplateUri :: String -> Either P.ParseError StarterTemplateId
    parseGhRepoTemplateUri = P.parse parser ""
      where
        parser = do
          _ <- P.string ghRepoTemplateIdPrefix
          repoOwner <- P.many1 (P.noneOf [FP.Posix.pathSeparator])
          repoName <- P.many1 (P.noneOf [FP.Posix.pathSeparator])
          maybeTmplDirStrongPath <-
            P.optionMaybe (P.char FP.Posix.pathSeparator >> P.many1 P.anyChar) >>= \case
              Nothing -> pure Nothing
              -- NOTE: Even though parseRelDir returns System Path, it is able to parse both Posix and System
              --   separators, which enables us to use it here even though we expect Posix.
              Just tmplDirFilePath -> either (fail . show) (pure . Just) $ parseRelDir tmplDirFilePath
          return $ GhRepoTemplateUri repoOwner repoName maybeTmplDirStrongPath

    ghRepoTemplateIdPrefix :: String
    ghRepoTemplateIdPrefix = "github:"

-- TODO: I don't like that we are relying on Show here for search.
--   Either name should reflect that, or we shouldn't use Show but name directly or id or something.
--   Yeah, I think we should go for implementing `getTemplateName` function.
findTemplateByName :: [ST.StarterTemplate] -> String -> Maybe ST.StarterTemplate
findTemplateByName templates templateName = find ((== templateName) . show) templates
