module StrongPathTest where

import Test.Tasty.Hspec

import qualified Path as P
import StrongPath
    ( Path, Abs, Dir, File, (</>)
    , fromRelFile, fromRelDir, fromAbsFile, fromAbsDir
    , toAbsFile, toAbsDir, toRelFile, toRelDir
    )


data Bar
data Fizz

spec_StrongPath :: Spec
spec_StrongPath = do
    describe "Example with Foo file and Bar, Fizz and Kokolo dirs" $ do
        let fooFileInBarDir = (fromRelFile [P.relfile|foo.txt|]) :: Path (Dir Bar) File
        let barDirInFizzDir = (fromRelDir [P.reldir|kokolo/bar|]) :: Path (Dir Fizz) (Dir Bar)
        let fizzDir = (fromAbsDir [P.absdir|/fizz|]) :: Path Abs (Dir Fizz)
        let fooFile = (fizzDir </> barDirInFizzDir </> fooFileInBarDir) :: Path Abs File
        let fooFileInFizzDir = (barDirInFizzDir </> fooFileInBarDir) :: Path (Dir Fizz) File

        it "Paths are correctly concatenated" $ do
            (P.toFilePath $ toAbsFile fooFile) `shouldBe` "/fizz/kokolo/bar/foo.txt"
            (P.toFilePath $ toRelFile fooFileInFizzDir) `shouldBe` "kokolo/bar/foo.txt"

    it "Paths are unchanged when packed and unpacked" $ do
        (P.toFilePath $ toRelFile $ fromRelFile [P.relfile|some/file.txt|]) `shouldBe` "some/file.txt"
        (P.toFilePath $ toRelDir $ fromRelDir [P.reldir|some/dir/|]) `shouldBe` "some/dir/"
        (P.toFilePath $ toAbsFile $ fromAbsFile [P.absfile|/some/file.txt|]) `shouldBe` "/some/file.txt"
        (P.toFilePath $ toAbsDir $ fromAbsDir [P.absdir|/some/dir/|]) `shouldBe` "/some/dir/"
