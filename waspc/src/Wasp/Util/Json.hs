module Wasp.Util.Json (parseJsonWithComments) where

import Data.Aeson (FromJSON)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import Wasp.Util.Aeson (decodeFromString)

-- | Uses Node.js to parse JSON with comments by treating it as a JavaScript object.
parseJsonWithComments :: FromJSON a => String -> IO (Either String a)
parseJsonWithComments jsonStr = do
  let evalScript = "const v = " ++ jsonStr ++ ";console.log(JSON.stringify(v));"
  let cp = P.proc "node" ["-e", evalScript]
  (exitCode, response, stderr) <- P.readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> return $ decodeFromString response
    _exitFailure -> return $ Left stderr
