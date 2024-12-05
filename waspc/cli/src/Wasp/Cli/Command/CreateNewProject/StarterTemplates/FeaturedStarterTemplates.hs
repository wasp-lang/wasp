module Wasp.Cli.Command.CreateNewProject.StarterTemplates.FeaturedStarterTemplates
  ( getFeaturedStarterTemplates,
    defaultStarterTemplate,
    aiGeneratedStarterTemplate,
  )
where

import StrongPath (Dir', Path', Rel', reldir)
import qualified System.FilePath as FP
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common (styleCode, styleText, waspVersionTemplateGitTag)
import qualified Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplate as ST
import qualified Wasp.Cli.GithubRepo as GhRepo

getFeaturedStarterTemplates :: [ST.StarterTemplate]
getFeaturedStarterTemplates =
  [ defaultStarterTemplate,
    todoTsStarterTemplate,
    openSaasStarterTemplate,
    embeddingsStarterTemplate,
    aiGeneratedStarterTemplate
  ]

defaultStarterTemplate :: ST.StarterTemplate
defaultStarterTemplate = basicStarterTemplate

{- HLINT ignore basicStarterTemplate "Redundant $" -}
basicStarterTemplate :: ST.StarterTemplate
basicStarterTemplate =
  ST.LocalStarterTemplate $
    ST.DirBasedTemplateMetadata
      { ST._path = [reldir|basic|],
        ST._name = "basic",
        ST._description = "Simple starter template with a single page.",
        ST._buildStartingInstructions = \projectDirName ->
          unlines
            [ styleText $ "To run your new app, do:",
              styleCode $ "    cd " <> projectDirName,
              styleCode $ "    wasp start"
            ]
      }

{- HLINT ignore openSaasStarterTemplate "Redundant $" -}
openSaasStarterTemplate :: ST.StarterTemplate
openSaasStarterTemplate =
  makeSimpleWaspGhRepoTemplate
    ("open-saas", [reldir|template|])
    ( "saas",
      "Everything a SaaS needs! Comes with Auth, ChatGPT API, Tailwind, Stripe payments and more."
        <> " Check out https://opensaas.sh/ for more details."
    )
    ( \projectDirName ->
        unlines
          [ styleText $ "To run your new app, follow the instructions below:",
            styleText $ "",
            styleText $ "  1. Position into app's root directory:",
            styleCode $ "    cd " <> projectDirName FP.</> "app",
            styleText $ "",
            styleText $ "  2. Run the development database (and leave it running):",
            styleCode $ "    wasp db start",
            styleText $ "",
            styleText $ "  3. Open new terminal window (or tab) in that same dir and continue in it.",
            styleText $ "",
            styleText $ "  4. Apply initial database migrations:",
            styleCode $ "    wasp db migrate-dev",
            styleText $ "",
            styleText $ "  5. Create initial dot env file from the template:",
            styleCode $ "    cp .env.server.example .env.server",
            styleText $ "",
            styleText $ "  6. Last step: run the app!",
            styleCode $ "    wasp start",
            styleText $ "",
            styleText $ "Check the README for additional guidance and the link to docs!"
          ]
    )

{- HLINT ignore todoTsStarterTemplate "Redundant $" -}
todoTsStarterTemplate :: ST.StarterTemplate
todoTsStarterTemplate =
  makeSimpleWaspGhRepoTemplate
    ("starters", [reldir|todo-ts|])
    ( "todo-ts",
      "Simple but well-rounded Wasp app implemented with Typescript & full-stack type safety."
    )
    ( \projectDirName ->
        unlines
          [ styleText $ "To run your new app, do:",
            styleCode $ "    cd " ++ projectDirName,
            styleCode $ "    wasp db migrate-dev",
            styleCode $ "    wasp start",
            styleText $ "",
            styleText $ "Check the README for additional guidance!"
          ]
    )

{- HLINT ignore embeddingsStarterTemplate "Redundant $" -}
embeddingsStarterTemplate :: ST.StarterTemplate
embeddingsStarterTemplate =
  makeSimpleWaspGhRepoTemplate
    ("starters", [reldir|embeddings|])
    ( "embeddings",
      "Comes with code for generating vector embeddings and performing vector similarity search."
    )
    ( \projectDirName ->
        unlines
          [ styleText $ "To run your new app, follow the instructions below:",
            styleText $ "",
            styleText $ "  1. Position into app's root directory:",
            styleCode $ "    cd " <> projectDirName,
            styleText $ "",
            styleText $ "  2. Create initial dot env file from the template and fill in your API keys:",
            styleCode $ "    cp .env.server.example .env.server",
            styleText $ "    Fill in your API keys!",
            styleText $ "",
            styleText $ "  3. Run the development database (and leave it running):",
            styleCode $ "    wasp db start",
            styleText $ "",
            styleText $ "  4. Open new terminal window (or tab) in that same dir and continue in it.",
            styleText $ "",
            styleText $ "  5. Apply initial database migrations:",
            styleCode $ "    wasp db migrate-dev",
            styleText $ "",
            styleText $ "  6. Run wasp seed script that will generate embeddings from the text files in src/shared/docs:",
            styleCode $ "    wasp db seed",
            styleText $ "",
            styleText $ "  7. Last step: run the app!",
            styleCode $ "    wasp start",
            styleText $ "",
            styleText $ "Check the README for more detailed instructions and additional guidance!"
          ]
    )

{- HLINT ignore aiGeneratedStarterTemplate "Redundant $" -}
aiGeneratedStarterTemplate :: ST.StarterTemplate
aiGeneratedStarterTemplate =
  ST.AiGeneratedStarterTemplate $
    ST.TemplateMetadata
      { _tmplName = "ai-generated",
        _tmplDescription = "🤖 Describe an app in a couple of sentences and have Wasp AI generate initial code for you. (experimental)",
        _tmplBuildStartingInstructions = \projectDirName ->
          unlines
            [ styleText $ "To run your new app, do:",
              styleCode $ "    cd " <> projectDirName,
              styleCode $ "    wasp db migrate-dev",
              styleCode $ "    wasp start"
            ]
      }

makeSimpleWaspGhRepoTemplate :: (String, Path' Rel' Dir') -> (String, String) -> ST.StartingInstructionsBuilder -> ST.StarterTemplate
makeSimpleWaspGhRepoTemplate (repoName, tmplPathInRepo) (tmplDisplayName, tmplDescription) buildStartingInstructions =
  ST.GhRepoStarterTemplate
    ( GhRepo.GithubRepoRef
        { GhRepo._repoOwner = waspGhOrgName,
          GhRepo._repoName = repoName,
          GhRepo._repoReferenceName = waspVersionTemplateGitTag
        }
    )
    ( ST.DirBasedTemplateMetadata
        { _name = tmplDisplayName,
          _description = tmplDescription,
          _path = tmplPathInRepo,
          _buildStartingInstructions = buildStartingInstructions
        }
    )

waspGhOrgName :: String
waspGhOrgName = "wasp-lang"
