{-# LANGUAGE DuplicateRecordFields #-}

module Command
  ( Command (..),
    CommandCwd (..),
    cmd,
    showCommand,
    programFromEnvVar,
    withEnvVars,
    withoutEnvVars,
    withStdin,
    inRelativeDir,
    inAbsoluteDir,
    CommandResult (..),
    executeCommand,
    StartedCommand (..),
    startCommand,
  )
where

import Control.Concurrent.Async (async, cancel, wait)
import Control.Exception (bracketOnError)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import StrongPath (Abs, Dir, Dir', Path', Rel, Rel', castDir, castRel, fromAbsDir, (</>))
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import qualified System.Process as P
import TestLogger (TestLogger, logOutputChunk)

-- | A command to be executed as a separate process (without a shell).
-- Commands are plain values, so wrappers like 'withEnvVars' or expecting
-- failure can be composed before the command is run as a test step.
data Command = Command
  { -- | Name of the program, looked up on @PATH@ (e.g. @wasp-cli@, @npx@, @docker@).
    program :: String,
    -- | When set, the program is read from this environment variable at run
    -- time, falling back to 'program' if the variable is unset. See
    -- 'programFromEnvVar'.
    programEnvVarOverride :: Maybe String,
    args :: [String],
    -- | When set, this text is piped to the process's stdin (which is then closed).
    stdinText :: Maybe T.Text,
    -- | Environment variables added on top of the inherited environment.
    addedEnvVars :: [(String, String)],
    -- | Environment variables removed from the inherited environment.
    removedEnvVars :: [String],
    -- | When set, overrides the working directory the command runs in
    -- (by default, commands run in the step context's working directory).
    cwdOverride :: Maybe CommandCwd
  }

data CommandCwd
  = CommandCwdAbs (Path' Abs Dir')
  | CommandCwdRelativeToWorkingDir (Path' Rel' Dir')

cmd :: String -> [String] -> Command
cmd program args =
  Command
    { program,
      programEnvVarOverride = Nothing,
      args,
      stdinText = Nothing,
      addedEnvVars = [],
      removedEnvVars = [],
      cwdOverride = Nothing
    }

-- | Human-readable rendering of the command, for logs and failure messages.
showCommand :: Command -> String
showCommand command =
  concat
    [ concatMap (\envVarName -> "unset " ++ envVarName ++ "; ") command.removedEnvVars,
      concatMap (\(name, value) -> name ++ "=" ++ quoteForDisplay value ++ " ") command.addedEnvVars,
      unwords $ map quoteForDisplay (command.program : command.args)
    ]

quoteForDisplay :: String -> String
quoteForDisplay value
  | null value = "''"
  | all isSafeChar value = value
  | otherwise = "'" ++ concatMap escapeSingleQuote value ++ "'"
  where
    isSafeChar char = isAlphaNum char || char `elem` ("_@%+=:,./-" :: String)

    escapeSingleQuote '\'' = "'\\''"
    escapeSingleQuote char = [char]

-- | Resolves the program from the given environment variable at run time,
-- falling back to the literal 'program' when the variable is unset. Used to run
-- the dev Wasp CLI through @WASP_CLI_CMD@ (a @cabal run@ wrapper that is a
-- single executable standing in for @wasp-cli@).
programFromEnvVar :: String -> Command -> Command
programFromEnvVar envVarName command = command {programEnvVarOverride = Just envVarName}

withEnvVars :: [(String, String)] -> Command -> Command
withEnvVars envVars command = command {addedEnvVars = envVars ++ command.addedEnvVars}

withoutEnvVars :: [String] -> Command -> Command
withoutEnvVars envVarNames command = command {removedEnvVars = envVarNames ++ command.removedEnvVars}

withStdin :: T.Text -> Command -> Command
withStdin stdinText command = command {stdinText = Just stdinText}

inRelativeDir :: Path' (Rel d) (Dir d') -> Command -> Command
inRelativeDir dir command =
  command {cwdOverride = Just $ CommandCwdRelativeToWorkingDir $ castDir $ castRel dir}

inAbsoluteDir :: Path' Abs (Dir d) -> Command -> Command
inAbsoluteDir dir command = command {cwdOverride = Just $ CommandCwdAbs $ castDir dir}

data CommandResult = CommandResult
  { exitCode :: ExitCode,
    stdoutText :: T.Text,
    stderrText :: T.Text,
    -- | Stdout and stderr interleaved in arrival order (close to what
    -- @2>&1@ would produce).
    combinedOutput :: T.Text
  }

-- | A spawned command whose completion can be awaited or which can be terminated
-- early. Termination kills the whole process group, so child processes (e.g.
-- @npm@ spawned by @wasp-cli@) don't outlive the test.
data StartedCommand = StartedCommand
  { waitForResult :: IO CommandResult,
    terminate :: IO ()
  }

-- | Runs a command to completion in the given working directory, streaming its
-- output to the test log and capturing it for assertions. If the executing
-- thread is interrupted (e.g. a concurrent test failed), the command's whole
-- process group is terminated.
executeCommand :: TestLogger -> Path' Abs Dir' -> Command -> IO CommandResult
executeCommand logger commandWorkingDir command =
  bracketOnError
    (startCommand logger commandWorkingDir command)
    (.terminate)
    (.waitForResult)

startCommand :: TestLogger -> Path' Abs Dir' -> Command -> IO StartedCommand
startCommand logger commandWorkingDir command = do
  envVars <- resolveEnvVars
  resolvedProgram <- resolveProgram
  (maybeStdinHandle, maybeStdoutHandle, maybeStderrHandle, processHandle) <-
    P.createProcess
      (P.proc resolvedProgram command.args)
        { P.cwd = Just $ fromAbsDir resolvedCwd,
          P.env = Just envVars,
          P.std_in = maybe P.Inherit (const P.CreatePipe) command.stdinText,
          P.std_out = P.CreatePipe,
          P.std_err = P.CreatePipe,
          P.create_group = True
        }

  writeStdin maybeStdinHandle

  combinedChunksRef <- newIORef []
  stdoutDrain <- async $ drainHandle combinedChunksRef (expectHandle maybeStdoutHandle)
  stderrDrain <- async $ drainHandle combinedChunksRef (expectHandle maybeStderrHandle)

  return
    StartedCommand
      { waitForResult = do
          stdoutBytes <- wait stdoutDrain
          stderrBytes <- wait stderrDrain
          exitCode <- P.waitForProcess processHandle
          combinedBytes <- BS.concat . reverse <$> readIORef combinedChunksRef
          return
            CommandResult
              { exitCode,
                stdoutText = decodeOutput stdoutBytes,
                stderrText = decodeOutput stderrBytes,
                combinedOutput = decodeOutput combinedBytes
              },
        terminate = do
          P.interruptProcessGroupOf processHandle
          void $ P.waitForProcess processHandle
          cancel stdoutDrain
          cancel stderrDrain
      }
  where
    resolvedCwd = case command.cwdOverride of
      Nothing -> commandWorkingDir
      Just (CommandCwdAbs dir) -> dir
      Just (CommandCwdRelativeToWorkingDir dir) -> commandWorkingDir </> dir

    resolveProgram :: IO String
    resolveProgram = case command.programEnvVarOverride of
      Nothing -> return command.program
      Just envVarName -> fromMaybe command.program <$> lookupEnv envVarName

    resolveEnvVars :: IO [(String, String)]
    resolveEnvVars = do
      inheritedEnvVars <- getEnvironment
      let overriddenEnvVarNames = command.removedEnvVars ++ map fst command.addedEnvVars
      return $
        command.addedEnvVars
          ++ filter ((`notElem` overriddenEnvVarNames) . fst) inheritedEnvVars

    writeStdin :: Maybe Handle -> IO ()
    writeStdin maybeStdinHandle = case (command.stdinText, maybeStdinHandle) of
      (Just stdinText, Just stdinHandle) -> do
        BS.hPut stdinHandle $ TE.encodeUtf8 stdinText
        hClose stdinHandle
      _ -> return ()

    -- Reads the handle until EOF, teeing each chunk to the test log and to the
    -- shared combined-output accumulator, and returns the handle's full output.
    drainHandle :: IORef [BS.ByteString] -> Handle -> IO BS.ByteString
    drainHandle combinedChunksRef handle = go []
      where
        go chunks = do
          chunk <- BS.hGetSome handle 4096
          if BS.null chunk
            then return $ BS.concat $ reverse chunks
            else do
              logOutputChunk logger chunk
              atomicModifyIORef' combinedChunksRef (\allChunks -> (chunk : allChunks, ()))
              go (chunk : chunks)

    expectHandle :: Maybe Handle -> Handle
    expectHandle (Just handle) = handle
    expectHandle Nothing = error "Command: expected a pipe handle for the spawned process."

    decodeOutput :: BS.ByteString -> T.Text
    decodeOutput = TE.decodeUtf8With lenientDecode
