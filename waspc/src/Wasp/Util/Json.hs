module Wasp.Util.Json
  ( parseJsonWithComments,
    updateJsonFile,
    readJsonFile,
    writeJsonFile,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value (..), decodeFileStrict, eitherDecode, encodeFile)
import StrongPath (Abs, File, Path', fromAbsFile)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import Wasp.Util.Aeson (decodeFromString)
import qualified Wasp.Util.IO as IOUtil

-- | Uses Node.js to parse JSON with comments by treating it as a JavaScript object.
-- We use this technique because Aeson can't read JSON with comments and we didn't want to write
-- a custom parser.
parseJsonWithComments :: (FromJSON a) => String -> IO (Either String a)
parseJsonWithComments jsonStr = do
  let evalScript = "const v = " ++ jsonStr ++ ";console.log(JSON.stringify(v));"
  let cp = P.proc "node" ["-e", evalScript]
  (exitCode, response, stderr) <- P.readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> return $ decodeFromString response
    _exitFailure -> return $ Left stderr

updateJsonFile :: (Value -> Value) -> Path' Abs (File a) -> IO (Either String ())
updateJsonFile updateFn jsonFilePath = runExceptT $ do
  jsonContent <- ExceptT $ eitherDecode <$> IOUtil.readFileBytes jsonFilePath
  liftIO $ writeJsonFile jsonFilePath $ updateFn jsonContent

readJsonFile :: (FromJSON a) => Path' Abs (File f) -> IO (Maybe a)
readJsonFile = decodeFileStrict . fromAbsFile

writeJsonFile :: (ToJSON a) => Path' Abs (File f) -> a -> IO ()
writeJsonFile = encodeFile . fromAbsFile
