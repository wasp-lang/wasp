module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplate (..),
    TemplateMetadata (..),
    findTemplateByString,
    getTemplateStartingInstructions,
    skeletonDotfiles,
  )
where

import Data.Foldable (find)
import StrongPath (Dir', Path', Rel')
import qualified Wasp.Cli.GithubRepo as GhRepo
import qualified Wasp.Cli.Interactive as Interactive

-- More on how starter templates work in Wasp, including the development process,
-- can be found in the `waspc/data/Cli/starters/README.md` file.

data StarterTemplate
  = -- | Template from an archive added to a named GitHub Release as an asset.
    GhRepoReleaseArchiveTemplate
      { repo :: !GhRepo.GithubRepoRef,
        archiveName :: !GhRepo.GithubReleaseArchiveName,
        archivePath :: Path' Rel' Dir',
        metadata :: !TemplateMetadata
      }
  | -- | Template from disk, that comes bundled with wasp CLI.
    BundledStarterTemplate
      { bundledPath :: Path' Rel' Dir',
        metadata :: !TemplateMetadata
      }

data TemplateMetadata = TemplateMetadata
  { _name :: !String,
    _description :: !String,
    -- | Given the name of the project directory, makes the starting instructions.
    _buildStartingInstructions :: !(String -> String)
  }

instance Show StarterTemplate where
  show (GhRepoReleaseArchiveTemplate {metadata = metadata'}) = _name metadata'
  show (BundledStarterTemplate {metadata = metadata'}) = _name metadata'

instance Interactive.IsOption StarterTemplate where
  showOption = show

  showOptionDescription (GhRepoReleaseArchiveTemplate {metadata = metadata'}) = Just $ _description metadata'
  showOptionDescription (BundledStarterTemplate {metadata = metadata'}) = Just $ _description metadata'

{- HLINT ignore getTemplateStartingInstructions "Redundant $" -}

-- | Returns instructions for running the newly created (from the template) Wasp project.
-- Instructions assume that user is positioned right next to the just created project directory,
-- whose name is provided via projectDirName.
getTemplateStartingInstructions :: String -> StarterTemplate -> String
getTemplateStartingInstructions projectDirName = \case
  GhRepoReleaseArchiveTemplate {metadata = metadata'} -> _buildStartingInstructions metadata' projectDirName
  BundledStarterTemplate {metadata = metadata'} -> _buildStartingInstructions metadata' projectDirName

findTemplateByString :: [StarterTemplate] -> String -> Maybe StarterTemplate
findTemplateByString templates query = find ((== query) . show) templates

-- | Files stored without their leading dot in the skeleton template directory.
-- They are stored this way to prevent tools (e.g. npm) from stripping them
-- during packaging. Both bundled and AI template paths use this list to
-- restore the leading dot.
skeletonDotfiles :: [String]
skeletonDotfiles = ["gitignore", "npmrc"]
