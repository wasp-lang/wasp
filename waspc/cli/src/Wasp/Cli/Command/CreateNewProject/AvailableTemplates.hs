{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Command.CreateNewProject.AvailableTemplates
  ( availableStarterTemplates,
    defaultStarterTemplate,
  )
where

import StrongPath (reldir)
import qualified System.FilePath as FP
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate (..), TemplateMetadata (..))
import qualified Wasp.Cli.GithubRepo as GhRepo
import qualified Wasp.SemanticVersion as SV
import Wasp.Util.Terminal (styleCode, styleText)
import Wasp.Version (waspVersion)

availableStarterTemplates :: [StarterTemplate]
availableStarterTemplates =
  [ basicStarterTemplate,
    minimalStarterTemplate,
    openSaasStarterTemplate,
    AiGeneratedStarterTemplate
  ]

defaultStarterTemplate :: StarterTemplate
defaultStarterTemplate = basicStarterTemplate

{- HLINT ignore minimalStarterTemplate "Redundant $" -}

minimalStarterTemplate :: StarterTemplate
minimalStarterTemplate =
  BundledStarterTemplate
    { bundledPath = [reldir|minimal|],
      metadata =
        TemplateMetadata
          { _name = "minimal",
            _description = "A minimal starter template that features just a single page.",
            _buildStartingInstructions = \projectDirName ->
              unlines
                [ styleText $ "To run your new app, do:",
                  styleCode $ "    cd " <> projectDirName,
                  styleCode $ "    wasp start"
                ]
          }
    }

{- HLINT ignore basicStarterTemplate "Redundant $" -}

basicStarterTemplate :: StarterTemplate
basicStarterTemplate =
  BundledStarterTemplate
    { bundledPath = [reldir|basic|],
      metadata =
        TemplateMetadata
          { _name = "basic",
            _description = "A basic starter template designed to help you get up and running quickly. It features examples covering the most common use cases.",
            _buildStartingInstructions = \projectDirName ->
              unlines
                [ styleText $ "To run your new app, do:",
                  styleCode $ "    cd " <> projectDirName,
                  styleCode $ "    wasp db migrate-dev",
                  styleCode $ "    wasp start",
                  styleText $ "",
                  styleText $ "Check the " <> styleCode "README.md" <> " for additional guidance!"
                ]
          }
    }

{- HLINT ignore openSaasStarterTemplate "Redundant $" -}

openSaasStarterTemplate :: StarterTemplate
openSaasStarterTemplate =
  GhRepoReleaseArchiveTemplate
    { repo =
        ( GhRepo.GithubRepoRef
            { GhRepo._repoOwner = "wasp-lang",
              GhRepo._repoName = "open-saas",
              GhRepo._repoReferenceName = waspVersionTemplateGitTag
            }
        ),
      archiveName = "template.tar.gz",
      archivePath = [reldir|.|],
      metadata =
        ( TemplateMetadata
            { _name = "saas",
              _description =
                "Everything a SaaS needs! Comes with Auth, ChatGPT API, Tailwind, Stripe payments and more."
                  <> " Check out https://opensaas.sh/ for more details.",
              _buildStartingInstructions = \projectDirName ->
                unlines
                  [ styleText $
                      "To run your new app, follow the instructions below:",
                    styleText $ "",
                    styleText $ "  1. Position into app's root directory:",
                    styleCode $ "    cd " <> projectDirName FP.</> "app",
                    styleText $ "",
                    styleText $
                      "  2. Run the development database (and leave it running):",
                    styleCode $ "    wasp db start",
                    styleText $ "",
                    styleText $
                      "  3. Open new terminal window (or tab) in that same dir and continue in it.",
                    styleText $ "",
                    styleText $ "  4. Apply initial database migrations:",
                    styleCode $ "    wasp db migrate-dev",
                    styleText $ "",
                    styleText $
                      "  5. Create initial dot env file from the template:",
                    styleCode $ "    cp .env.server.example .env.server",
                    styleText $ "",
                    styleText $ "  6. Last step: run the app!",
                    styleCode $ "    wasp start",
                    styleText $ "",
                    styleText $
                      "Check the README for additional guidance and the link to docs!"
                  ]
            }
        )
    }

-- | Git tag for external templates, constructed dynamically from the current Wasp version.
-- Example: `wasp-v0.19-template`.
waspVersionTemplateGitTag :: String
waspVersionTemplateGitTag =
  "wasp-v" ++ show major ++ "." ++ show minor ++ "-template"
  where
    (SV.Version major minor _) = waspVersion
