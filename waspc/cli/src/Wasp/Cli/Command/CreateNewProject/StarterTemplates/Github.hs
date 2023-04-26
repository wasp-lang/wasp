module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Github where

type GithubRepo = (GithubRepoOwner, GithubRepoName)

type GithubRepoOwner = String

type GithubRepoName = String

starterTemplateGithubRepo :: GithubRepo
starterTemplateGithubRepo = ("wasp-lang", "starters")
