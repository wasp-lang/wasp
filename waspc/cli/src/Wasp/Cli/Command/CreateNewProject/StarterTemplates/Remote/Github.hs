module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github where

import Wasp.Cli.GithubRepo (GithubRepoRef (..))

starterTemplateGithubRepo :: GithubRepoRef
starterTemplateGithubRepo =
  GithubRepoRef
    { _repoOwner = "wasp-lang",
      _repoName = "starters",
      _repoReferenceName = "main"
    }
