module Wasp.Cli.Command.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    File,
    AuthProvider (..),
  )
where

import Data.Text (Text)

data NewProjectDetails = NewProjectDetails
  { _projectAppName :: !String,
    _projectDescription :: !String,
    _projectAuth :: !AuthProvider
  }

-- TODO: Make these relative to WaspProjectDir?
type File = (FilePath, Text)

-- TODO: Support more methods.
data AuthProvider = UsernameAndPassword
