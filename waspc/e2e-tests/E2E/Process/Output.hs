-- | Capturing the output of a process while it runs.
module E2E.Process.Output
  ( Output,
    Stream (..),
    newOutput,
    captureStream,
    readStdout,
    readCombinedOutput,
    isFullyRead,
  )
where

import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Control.Exception (finally)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import E2E.TestM (TestLog, writeToLog)
import System.IO (BufferMode (NoBuffering), Handle, hClose, hSetBuffering)
import qualified System.Process.Typed as TP
import UnliftIO.Exception (catchAny)

data Stream = Stdout | Stderr

data Output = Output
  { testLog :: TestLog,
    -- | Chunks of both streams in the order they arrived, newest first.
    chunks :: TVar [(Stream, BS.ByteString)],
    -- | How many streams are still being read.
    openStreams :: TVar Int
  }

newOutput :: TestLog -> IO Output
newOutput testLog = Output testLog <$> newTVarIO [] <*> newTVarIO 0

-- | Reads the stream in a thread, which also writes it to the test log, and
-- closes the pipe once it's done. Closing it from elsewhere would wait for a
-- read in progress, which lasts as long as any children that outlive the
-- process keep the pipe open.
captureStream :: Output -> Stream -> TP.StreamSpec streamType ()
captureStream output stream = TP.mkPipeStreamSpec $ \_ handle -> do
  atomically $ modifyTVar' output.openStreams (+ 1)
  _ <- forkIOWithUnmask $ \unmask -> unmask (readChunks handle) `finally` closeStream handle
  return ((), return ())
  where
    readChunks :: Handle -> IO ()
    readChunks handle = do
      hSetBuffering handle NoBuffering
      let loop = do
            chunk <- BS.hGetSome handle 32768
            unless (BS.null chunk) $ do
              atomically $ modifyTVar' output.chunks ((stream, chunk) :)
              writeToLog output.testLog chunk
              loop
      loop `catchAny` const (return ())

    closeStream :: Handle -> IO ()
    closeStream handle = do
      hClose handle `catchAny` const (return ())
      atomically $ modifyTVar' output.openStreams (subtract 1)

readStdout :: Output -> STM T.Text
readStdout output = decodeChunks . (\chunks -> [chunk | (Stdout, chunk) <- chunks]) <$> readChunksInOrder output

-- | Stdout and stderr interleaved in the order they arrived, like @2>&1@.
readCombinedOutput :: Output -> STM T.Text
readCombinedOutput output = decodeChunks . map snd <$> readChunksInOrder output

-- | Both streams have been read to the end.
isFullyRead :: Output -> STM Bool
isFullyRead output = (== 0) <$> readTVar output.openStreams

readChunksInOrder :: Output -> STM [(Stream, BS.ByteString)]
readChunksInOrder output = reverse <$> readTVar output.chunks

decodeChunks :: [BS.ByteString] -> T.Text
decodeChunks = TE.decodeUtf8With TE.lenientDecode . BS.concat
