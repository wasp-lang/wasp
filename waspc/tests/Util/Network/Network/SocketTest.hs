module Util.Network.SocketTest where

import Control.Exception (bracket)
import Data.Either (isLeft)
import Foreign.C.Error (Errno (..), eACCES, eCONNREFUSED, eTIMEDOUT)
import GHC.IO.Exception (IOException (..), IOErrorType (..))
import qualified Network.Socket as S
import Test.Hspec
import Wasp.Util.Network.Socket
  ( ConnectProbeResult (..),
    checkIfPortIsAcceptingConnections,
    classifyConnectException,
    makeLocalHostSocketAddress,
  )

spec_classifyConnectException :: Spec
spec_classifyConnectException = do
  it "classifies a refused connection as ConnectRefused" $ do
    classifyConnectException (connectErrorWithErrno eCONNREFUSED)
      `shouldBe` Right ConnectRefused
    classifyConnectException (connectErrorWithDescription "connect: WSAECONNREFUSED")
      `shouldBe` Right ConnectRefused

  it "classifies a timed out connection as ConnectTimedOut" $ do
    classifyConnectException (connectErrorWithErrno eTIMEDOUT)
      `shouldBe` Right ConnectTimedOut
    classifyConnectException (connectErrorWithDescription "connect: WSAETIMEDOUT")
      `shouldBe` Right ConnectTimedOut

  it "leaves indeterminate failures as exceptions" $ do
    isLeft (classifyConnectException (connectErrorWithErrno eACCES)) `shouldBe` True
    isLeft (classifyConnectException (connectErrorWithDescription "some other failure"))
      `shouldBe` True
  where
    connectErrorWithErrno :: Errno -> IOException
    connectErrorWithErrno (Errno errno) = baseConnectError {ioe_errno = Just errno}

    connectErrorWithDescription :: String -> IOException
    connectErrorWithDescription description = baseConnectError {ioe_description = description}

    baseConnectError :: IOException
    baseConnectError =
      IOError
        { ioe_handle = Nothing,
          ioe_type = OtherError,
          ioe_location = "connect",
          ioe_description = "",
          ioe_errno = Nothing,
          ioe_filename = Nothing
        }

-- We test with fixed ports below the range the OS hands out ephemeral ports
-- from (it starts at 32768), assuming they are free, and use ports distinct
-- from the ones the cli test suite uses, so tests can run in parallel.
spec_checkIfPortIsAcceptingConnections :: Spec
spec_checkIfPortIsAcceptingConnections = do
  it "returns True when the port is accepting connections" $
    withListeningSocketOn 20800 $
      checkIfPortIsAcceptingConnections (makeLocalHostSocketAddress 20800)
        `shouldReturn` True

  it "returns False when the connection is refused" $
    -- Nothing is listening on this port, so the connection is refused.
    checkIfPortIsAcceptingConnections (makeLocalHostSocketAddress 20801)
      `shouldReturn` False

withListeningSocketOn :: S.PortNumber -> IO a -> IO a
withListeningSocketOn port action =
  bracket openListeningSocket S.close (const action)
  where
    openListeningSocket = do
      sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
      S.bind sock $ makeLocalHostSocketAddress port
      S.listen sock 1
      return sock
