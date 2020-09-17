module StrongPathTest where

import Test.Tasty.Hspec
import qualified Path as P

import StrongPath (Path, Abs, Rel, Dir, File, (</>))
import qualified StrongPath as SP
import Fixtures (fpRoot)
import Test.Util (posixToSystemFp)


data Bar
data Fizz

spec_StrongPath :: Spec
spec_StrongPath = do
    describe "Example with Foo file and Bar, Fizz and Kokolo dirs" $ do
        let fooFileInBarDir = (SP.fromPathRelFile [P.relfile|foo.txt|]) :: Path (Rel Bar) File
        let barDirInFizzDir = (SP.fromPathRelDir [P.reldir|kokolo/bar|]) :: Path (Rel Fizz) (Dir Bar)
        let fizzDir = (SP.fromPathAbsDir $ fpRoot P.</> [P.reldir|fizz|]) :: Path Abs (Dir Fizz)
        let fooFile = (fizzDir </> barDirInFizzDir </> fooFileInBarDir) :: Path Abs File
        let fooFileInFizzDir = (barDirInFizzDir </> fooFileInBarDir) :: Path (Rel Fizz) File

        it "Paths are correctly concatenated" $ do
            (P.toFilePath $ SP.toPathAbsFile fooFile) `shouldBe` posixToSystemFp "/fizz/kokolo/bar/foo.txt"
            (P.toFilePath $ SP.toPathRelFile fooFileInFizzDir) `shouldBe` posixToSystemFp "kokolo/bar/foo.txt"

    it "Paths are unchanged when packed and unpacked" $ do
        (P.toFilePath $ SP.toPathRelFile $ SP.fromPathRelFile [P.relfile|some/file.txt|])
            `shouldBe` posixToSystemFp "some/file.txt"
        (P.toFilePath $ SP.toPathRelDir $ SP.fromPathRelDir [P.reldir|some/dir/|])
            `shouldBe` posixToSystemFp "some/dir/"
        (P.toFilePath $ SP.toPathAbsFile $ SP.fromPathAbsFile $ fpRoot P.</> [P.relfile|some/file.txt|])
            `shouldBe` posixToSystemFp "/some/file.txt"
        (P.toFilePath $ SP.toPathAbsDir $ SP.fromPathAbsDir $ fpRoot P.</> [P.reldir|some/dir/|])
            `shouldBe` posixToSystemFp "/some/dir/"
