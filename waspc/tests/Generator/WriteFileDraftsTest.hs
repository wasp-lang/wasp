{-# OPTIONS_GHC -Wno-orphans #-}

module Generator.WriteFileDraftsTest where

import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified StrongPath as SP
import Test.Tasty.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldMatchList, shouldReturn, shouldThrow)
import Wasp.Generator.FileDraft (FileDraft (FileDraftTextFd), Writeable (getDstPath))
import Wasp.Generator.FileDraft.TextFileDraft (TextFileDraft)
import qualified Wasp.Generator.FileDraft.TextFileDraft as TextFD
import Wasp.Generator.WriteFileDrafts (assertDstPathsAreUnique, fileDraftsToWriteAndFilesToDelete)
import Wasp.Util (Checksum, checksumFromString, checksumFromText)

genMockTextFileDrafts :: Int -> [TextFileDraft]
genMockTextFileDrafts n =
  let genMockTextFileDraft i =
        TextFD.TextFileDraft
          { TextFD._dstPath = fromJust $ SP.parseRelFile ("c/d/dst" ++ show i ++ ".txt"),
            TextFD._content = pack $ "Test" ++ show i
          }
   in take n (map genMockTextFileDraft [1 :: Int ..])

genFdsWithChecksums :: Int -> [(FileDraft, Checksum)]
genFdsWithChecksums n =
  map (\fd -> (FileDraftTextFd fd, checksumFromText $ TextFD._content fd)) (genMockTextFileDrafts n)

spec_WriteDuplicatedDstFileDrafts :: Spec
spec_WriteDuplicatedDstFileDrafts =
  describe "fileDraftsWithDuplicatedDstPaths" $ do
    it "should throw error since there are duplicated destination paths" $
      let fileDrafts = replicate 2 $ FileDraftTextFd $ head (genMockTextFileDrafts 1)
       in (return $! assertDstPathsAreUnique fileDrafts) `shouldThrow` anyErrorCall
    it "should not throw error because unique destination paths" $
      let fileDrafts = map FileDraftTextFd (genMockTextFileDrafts 2)
       in (return $! assertDstPathsAreUnique fileDrafts) `shouldReturn` ()

-- NOTE: Very weak show function, but it is good enough for the tests below.
instance Show FileDraft where
  show fd = "FileDraft {dstPath = " ++ show (getDstPath fd) ++ "}"

-- NOTE: Very weak eq function, but it is good enough for the tests below.
instance Eq FileDraft where
  fd1 == fd2 = getDstPath fd1 == getDstPath fd2

spec_WriteFileDrafts :: Spec
spec_WriteFileDrafts =
  describe "fileDraftsToWriteAndFilesToDelete" $ do
    it "should write and delete nothing if there are no checksums and no file drafts" $
      fileDraftsToWriteAndFilesToDelete Nothing [] `shouldBe` ([], [])
    it "should write all file drafts and delete nothing if there are no checksums" $
      fileDraftsToWriteAndFilesToDelete Nothing (genFdsWithChecksums 3) `shouldBe` (fst <$> genFdsWithChecksums 3, [])
    it "should write and delete nothing if checksums on disk match file drafts" $ do
      let fdsWithChecksums = genFdsWithChecksums 2
      let relPathsToChecksums = map (first getDstPath) fdsWithChecksums
      fileDraftsToWriteAndFilesToDelete (Just relPathsToChecksums) fdsWithChecksums `shouldBe` ([], [])
    it "should write new (not in checksums list) and updated (in checksums list but different checksum) files drafts and delete redundant files (in checksums but have no corresponding file draft)" $ do
      let fdsWithChecksums = genFdsWithChecksums 6
      let (newFdsWithChecksums, updatedFdsWithChecksums, unchangedFdsWithChecksums) = splitInto3Groups fdsWithChecksums
      let deletedRelPathsToChecksums =
            [ (Left [SP.relfile|path/to/redundant/file.txt|], checksumFromString "foofile"),
              (Right [SP.reldir|path/to/redundant/dir|], checksumFromString "foodir")
            ]
      let relPathsToChecksums =
            concat
              [ map (first getDstPath) unchangedFdsWithChecksums,
                map
                  ( \(fd, _) ->
                      let differentChecksum = checksumFromString $ fromEither SP.toFilePath SP.toFilePath (getDstPath fd) ++ "-footestbar"
                       in (getDstPath fd, differentChecksum)
                  )
                  updatedFdsWithChecksums,
                deletedRelPathsToChecksums
              ]
      let (fileDraftsToWrite, filesToDelete) = fileDraftsToWriteAndFilesToDelete (Just relPathsToChecksums) fdsWithChecksums
      fileDraftsToWrite `shouldMatchList` map fst (newFdsWithChecksums ++ updatedFdsWithChecksums)
      filesToDelete `shouldMatchList` map fst deletedRelPathsToChecksums
  where
    fromEither :: (a -> c) -> (b -> c) -> Either a b -> c
    fromEither f _ (Left a) = f a
    fromEither _ g (Right a) = g a

    -- NOTE: We could make math work better, including for lists < 3 elements,
    -- and possibly not divisible by 3. But this is internal helper so
    -- we can punt for now.
    splitInto3Groups :: [a] -> ([a], [a], [a])
    splitInto3Groups xs =
      case splitIntoChunks (length xs `div` 3) xs of
        [a, b, c] -> (a, b, c)
        _ -> error "fix this"

    splitIntoChunks :: Int -> [a] -> [[a]]
    splitIntoChunks _ [] = []
    splitIntoChunks n xs = take n xs : splitIntoChunks n (drop n xs)
