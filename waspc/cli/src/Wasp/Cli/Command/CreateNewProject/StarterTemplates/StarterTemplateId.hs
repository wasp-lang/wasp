module Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplateId
  ( getStarterTemplateByIdOrThrow,
    findTemplateByName,
  )
where

import Control.Arrow (left)
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import StrongPath (Dir', Path', Rel', parseRelDir, reldir)
import qualified System.FilePath.Posix as FP.Posix
import qualified Text.Parsec as P
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common (styleText, waspVersionTemplateGitTag)
import qualified Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplate as ST
import qualified Wasp.Cli.GithubRepo as GhRepo

-- | A way to uniquely reference a specific Wasp starter template.
data StarterTemplateId
  = -- | References one of the featured templates (templates that we know about here in Wasp CLI) by its name.
    FeaturedTemplateName !String
  | -- | References any template that is available as a github repo.
    -- Most useful for referencing third-party repos that we don't know about, since featured ones
    -- we can more simply reference by name.
    GhRepoTemplateUri !GhRepo.GithubRepoOwner !GhRepo.GithubRepoName !(Maybe (Path' Rel' Dir'))

data StarterTemplateIdType
  = IdTypeFeaturedTemplateName
  | IdTypeGhRepoTemplateUri
  deriving (Enum, Bounded)

-- TODO: Explain what is this type used for, and also ignore the warning about it not being used.
getStarterTemplateIdType :: StarterTemplateId -> StarterTemplateIdType
getStarterTemplateIdType = \case
  FeaturedTemplateName {} -> IdTypeFeaturedTemplateName
  GhRepoTemplateUri {} -> IdTypeGhRepoTemplateUri

getStarterTemplateIdTypeDescription :: StarterTemplateIdType -> String
getStarterTemplateIdTypeDescription = \case
  IdTypeFeaturedTemplateName -> "a featured template name (e.g. \"saas\")"
  IdTypeGhRepoTemplateUri -> "a github URI (github:<owner>/<repo>[/some/dir])"

-- | Given a template id (as string), it will obtain the information on the template that this id references.
-- It will throw if the id is invalid (can't be parsed, or information on the template can't be obtain based on it).
getStarterTemplateByIdOrThrow :: [ST.StarterTemplate] -> String -> Command ST.StarterTemplate
getStarterTemplateByIdOrThrow featuredTemplates templateIdString =
  (parseStarterTemplateId templateIdString & either throwTemplateIdParsingError pure) >>= \case
    FeaturedTemplateName templateName ->
      findTemplateByName featuredTemplates templateName
        & maybe (throwInvalidTemplateNameUsedError templateName) pure
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
  where
    throwTemplateIdParsingError errorMsg =
      throwProjectCreationError $
        "Failed to parse template id: " <> errorMsg <> "\n" <> expectedInputMessage

    throwInvalidTemplateNameUsedError templateName =
      throwProjectCreationError $
        "There is no featured template with name " <> templateName <> ".\n" <> expectedInputMessage

    -- TODO: Use getTemplateName here instead of `show`.
    expectedInputMessage =
      "Expected " <> intercalate " or " (getStarterTemplateIdTypeDescription <$> [minBound .. maxBound]) <> "."
        <> (" Valid featured template names are " <> intercalate ", " (show <$> featuredTemplates) <> ".")

parseStarterTemplateId :: String -> Either String StarterTemplateId
parseStarterTemplateId = \case
  templateId | ghRepoTemplateIdPrefix `isPrefixOf` templateId -> parseGhRepoTemplateUri templateId & left show
  templateId -> pure $ FeaturedTemplateName templateId
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
              -- Even though parseRelDir returns System Path, it is able to parse both Posix and System
              -- separators, which enables us to use it here even though we expect Posix.
              Just tmplDirFilePath -> either (fail . show) (pure . Just) $ parseRelDir tmplDirFilePath
          return $ GhRepoTemplateUri repoOwner repoName maybeTmplDirStrongPath

    ghRepoTemplateIdPrefix :: String
    ghRepoTemplateIdPrefix = "github:"

-- TODO: I don't like that we are relying on Show here for search.
--   Either name should reflect that, or we shouldn't use Show but name directly or id or something.
--   Yeah, I think we should go for implementing `getTemplateName` function.
findTemplateByName :: [ST.StarterTemplate] -> String -> Maybe ST.StarterTemplate
findTemplateByName templates templateName = find ((== templateName) . show) templates
