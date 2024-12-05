{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplateId
  ( getStarterTemplateByIdOrThrow,
    findTemplateByName,
  )
where

import Control.Arrow (left)
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import StrongPath (Dir', Path', Rel', parseRelDir, reldir)
import qualified System.FilePath.Posix as FP.Posix
import qualified Text.Parsec as P
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common (styleText, waspVersionTemplateGitTag)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplate (getTemplateName)
import qualified Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplate as ST
import qualified Wasp.Cli.GithubRepo as GhRepo

-- TODO: Update CLI instructions.
-- TODO: Add specifing of a third-party template under interactive choosing of template.

-- | A way to uniquely reference a specific Wasp starter template.
-- It is how users specify which starter template they want to use.
data StarterTemplateId
  = -- | References one of the featured templates (templates that we know about here in Wasp CLI) by its name.
    FeaturedTemplateName !String
  | -- | References any template that is available as a github repo.
    -- Most useful for referencing third-party repos that we don't know about, since featured ones
    -- we can more simply reference by name.
    GhRepoTemplateUri !GhRepo.GithubRepoOwner !GhRepo.GithubRepoName !(Maybe (Path' Rel' Dir'))

-- | This type allows us to reason about types of StarterTemplateIds without having their runtime values.
data StarterTemplateIdType
  = IdTypeFeaturedTemplateName
  | IdTypeGhRepoTemplateUri
  deriving (Enum, Bounded)

-- | This function serves its purpose just by being defined, even if not used anywhere, because it
-- ensures (via compiler warning) that we don't forget do update StarterTemplateIdType accordingly
-- when we change StarterTemplateId (create, delete or modify data constructor).
getStarterTemplateIdType :: StarterTemplateId -> StarterTemplateIdType
getStarterTemplateIdType = \case
  FeaturedTemplateName {} -> IdTypeFeaturedTemplateName
  GhRepoTemplateUri {} -> IdTypeGhRepoTemplateUri

getStarterTemplateIdTypeDescription :: StarterTemplateIdType -> String
getStarterTemplateIdTypeDescription = \case
  IdTypeFeaturedTemplateName -> "a featured template name (e.g. \"saas\")"
  IdTypeGhRepoTemplateUri -> "a github URI (github:<repo_owner>/<repo_name>[/dir/to/template])"

-- | Given a template id (as string), it will obtain the information on the template that this id references.
-- It will throw if the id is invalid (can't be parsed, or information on the template can't be obtain based on it).
getStarterTemplateByIdOrThrow :: (MonadError String m) => [ST.StarterTemplate] -> String -> m ST.StarterTemplate
getStarterTemplateByIdOrThrow featuredTemplates templateIdString =
  (parseStarterTemplateId templateIdString & either throwTemplateIdParsingError pure) >>= \case
    FeaturedTemplateName templateName ->
      findTemplateByName featuredTemplates templateName
        & maybe (throwInvalidTemplateNameUsedError templateName) pure
    GhRepoTemplateUri repoOwner repoName maybeTmplDirPath ->
      return $
        ST.GhRepoStarterTemplate
          (GhRepo.GithubRepoRef repoOwner repoName waspVersionTemplateGitTag)
          (maybeTmplDirPath & fromMaybe [reldir|.|])
          ( ST.TemplateMetadata
              { ST._tmplName = repoName,
                ST._tmplDescription = "Template from Github repo " <> repoOwner <> "/" <> repoName,
                ST._tmplBuildStartingInstructions = \_ ->
                  unlines
                    [ styleText $ "Check https://github.com/" <> repoOwner <> "/" <> repoName <> " for starting instructions."
                    ]
              }
          )
  where
    throwTemplateIdParsingError errorMsg =
      throwError $
        "Failed to parse template id: " <> errorMsg <> "\n" <> expectedInputMessage

    throwInvalidTemplateNameUsedError templateName =
      throwError $
        "There is no featured template with name " <> wrapInQuotes templateName <> ".\n" <> expectedInputMessage

    expectedInputMessage =
      "Expected " <> intercalate " or " (getStarterTemplateIdTypeDescription <$> [minBound .. maxBound]) <> "."
        <> ("\nValid featured template names are " <> intercalate ", " (wrapInQuotes . getTemplateName <$> featuredTemplates) <> ".")

    wrapInQuotes str = "\"" <> str <> "\""

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
          _ <- P.char FP.Posix.pathSeparator
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

findTemplateByName :: [ST.StarterTemplate] -> String -> Maybe ST.StarterTemplate
findTemplateByName templates templateName = find ((== templateName) . getTemplateName) templates
