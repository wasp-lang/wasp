module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common
  ( waspVersionTemplateGitTag,
    styleCode,
    styleText,
  )
where

import qualified Wasp.Util.Terminal as Term

-- NOTE: As version of Wasp CLI changes, so we should update this tag name here,
--   and also create it on gh repos of templates.
--   By tagging templates for each version of Wasp CLI, we ensure that each release of
--   Wasp CLI uses correct version of templates, that work with it.
waspVersionTemplateGitTag :: String
waspVersionTemplateGitTag = "wasp-v0.15-template"

-- * Functions for styling the template instructions. Their names are on purpose of same length, for nicer code formatting.

styleCode :: String -> String
styleCode = Term.applyStyles [Term.Bold]

styleText :: String -> String
styleText = id
