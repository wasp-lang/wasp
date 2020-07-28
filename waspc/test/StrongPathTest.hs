module StrongPathTest where

import Test.Tasty.Hspec

import qualified Path as P
import StrongPath (Path, Abs, Rel, Dir, File, (</>))
import qualified StrongPath as SP


data Bar
data Fizz

spec_StrongPath :: Spec
spec_StrongPath = do
    describe "Example with Foo file and Bar, Fizz and Kokolo dirs" $ do
        let fooFileInBarDir = (SP.fromPathRelFile [P.relfile|foo.txt|]) :: Path (Rel Bar) File
        let barDirInFizzDir = (SP.fromPathRelDir [P.reldir|kokolo/bar|]) :: Path (Rel Fizz) (Dir Bar)
        let fizzDir = (SP.fromPathAbsDir [P.absdir|/fizz|]) :: Path Abs (Dir Fizz)
        let fooFile = (fizzDir </> barDirInFizzDir </> fooFileInBarDir) :: Path Abs File
        let fooFileInFizzDir = (barDirInFizzDir </> fooFileInBarDir) :: Path (Rel Fizz) File

        it "Paths are correctly concatenated" $ do
            (P.toFilePath $ SP.toPathAbsFile fooFile) `shouldBe` "/fizz/kokolo/bar/foo.txt"
            (P.toFilePath $ SP.toPathRelFile fooFileInFizzDir) `shouldBe` "kokolo/bar/foo.txt"

    it "Paths are unchanged when packed and unpacked" $ do
        (P.toFilePath $ SP.toPathRelFile $ SP.fromPathRelFile [P.relfile|some/file.txt|]) `shouldBe` "some/file.txt"
        (P.toFilePath $ SP.toPathRelDir $ SP.fromPathRelDir [P.reldir|some/dir/|]) `shouldBe` "some/dir/"
        (P.toFilePath $ SP.toPathAbsFile $ SP.fromPathAbsFile [P.absfile|/some/file.txt|]) `shouldBe` "/some/file.txt"
        (P.toFilePath $ SP.toPathAbsDir $ SP.fromPathAbsDir [P.absdir|/some/dir/|]) `shouldBe` "/some/dir/"
