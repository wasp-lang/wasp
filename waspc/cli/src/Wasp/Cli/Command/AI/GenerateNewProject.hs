module Wasp.Cli.Command.AI.GenerateNewProject () where

import Wasp.OpenAI (OpenAIApiKey)
import qualified Wasp.OpenAI.ChatGPT as Chat

data NewProjectDetails = NewProjectDetails
  { _projectName :: !String,
    _projectDescription :: !String,
    _projectAuth :: !AuthProvider
  }

data AuthProvider = Google

data FileDraft = FileDraft
  { path :: FilePath, -- TODO: Use StrongPath?
    content :: Text
  }

-- TODO: Create a monad stack transformer that contains openaiapikey + has state which holds files to generate?
--   Call it CodeGenerator. It can also send messages (via Chan) about its progress.

-- TODO: Have generateNewProject accept Chan, to which it will stream its progress?
--   It could just stream its output instead of printing it to stdout, so calling function
--   has more control over what to do with it.
--   Yeah, I think we certainly want to have Chan. And one important thing we want to send over it
--   is information about files that we are updating. So each time we create a new file or update existing one,
--   we want to send over information about that, so receiver can either write it to disk, or show it in web app,
--   or something. Such messages could be called "FileWritten" and say which path, what is the new content,
--   and also contain description of what happened (or maybe that is separate message).
generateNewProject :: OpenAIApiKey -> NewProjectDetails -> IO ()
generateNewProject openAiKey newProjectDetails = do
  let waspFile = generateBaseWaspFile newProjectDetails
  let dotEnvServerFile = generateDotEnvServer newProjectDetails
  -- TODO: write generateOtherNewProjectFiles based on existing CNP.createWaspProjectDir function.
  let otherNewProjectFiles = generateOtherNewProjectFiles newProjectDetails
  -- TODO: Send these files over (via Chan): "I have created these files, and this is their content."
  plan <- generatePlan openAiKey newProjectDetails
  -- TODO: Send info about the plan we created. Not sure if we will want to show it though,
  --   maybe just some short summar of it (4 entities, 3 operations, ...).
  let entities = generateEntities newProjectDetails plan
  updateWaspFileWithEntities entities
  -- TODO: Send info about entities we generated. Also send updated Wasp file.
  actions <- generateActions newProjectDetails plan entities
  -- TODO: generateActions should send message with file changes due to created action after every action it generates.
  --   It will be modifying Wasp file, but also modifying JS files.
  queries <- generateQueries newProjectDetails plan entities
  -- TODO: Same as actions.
  pages <- generatePages newProjectDetails plan entities queries actions
  -- TODO: Similar as actions.
  return ()
