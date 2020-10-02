module StrongPathTest where

import           Test.Tasty.Hspec

import           Data.Maybe       (fromJust)
import qualified Path             as P
import qualified Path.Posix       as PP
import qualified Path.Windows     as PW
import qualified System.FilePath  as FP

import           Fixtures         (systemFpRoot, systemPathRoot)
import           StrongPath       (Abs, Dir, File, Path, Rel, (</>))
import qualified StrongPath       as SP
import           Test.Util        (posixToSystemFp, posixToWindowsFp)


data Bar
data Fizz

spec_StrongPath :: Spec
spec_StrongPath = do
    describe "Example with Foo file and Bar, Fizz and Kokolo dirs" $ do
        let fooFileInBarDir = (SP.fromPathRelFile [P.relfile|foo.txt|]) :: Path (Rel Bar) File
        let barDirInFizzDir = (SP.fromPathRelDir [P.reldir|kokolo/bar|]) :: Path (Rel Fizz) (Dir Bar)
        let fizzDir = (SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|fizz|]) :: Path Abs (Dir Fizz)
        let fooFile = (fizzDir </> barDirInFizzDir </> fooFileInBarDir) :: Path Abs File
        let fooFileInFizzDir = (barDirInFizzDir </> fooFileInBarDir) :: Path (Rel Fizz) File

        it "Paths are correctly concatenated" $ do
            (P.toFilePath $ SP.toPathAbsFile fooFile) `shouldBe` posixToSystemFp "/fizz/kokolo/bar/foo.txt"
            (P.toFilePath $ SP.toPathRelFile fooFileInFizzDir) `shouldBe` posixToSystemFp "kokolo/bar/foo.txt"

    it "Paths are unchanged when packed from Path.Path and unpacked to Path.Path" $ do
        let test pack unpack path = unpack (pack path) == path `shouldBe` True
        test SP.fromPathRelFile SP.toPathRelFile [P.relfile|some/file.txt|]
        test SP.fromPathRelDir  SP.toPathRelDir  [P.reldir|some/dir/|]
        test SP.fromPathAbsFile SP.toPathAbsFile $ systemPathRoot P.</> [P.relfile|some/file.txt|]
        test SP.fromPathAbsDir  SP.toPathAbsDir  $ systemPathRoot P.</> [P.reldir|some/file.txt|]

    describe "relDirToPosix/relFileToPosix correctly converts relative strong path to Posix" $ do
        it "correctly when strong path is relative dir" $ do
            let expectedPosixPath = SP.fromPathRelDirP [PP.reldir|test/dir/|]
            fromJust (SP.relDirToPosix $ SP.fromPathRelDirW [PW.reldir|test\dir\|])
                `shouldBe` expectedPosixPath
            fromJust (SP.relDirToPosix $ SP.fromPathRelDirP [PP.reldir|test/dir/|])
                `shouldBe` expectedPosixPath
            fromJust (SP.relDirToPosix $ SP.fromPathRelDir [P.reldir|test/dir/|])
                `shouldBe` expectedPosixPath
        it "correctly when strong path is relative file" $ do
            let expectedPosixPath = SP.fromPathRelFileP [PP.relfile|test/file|]
            fromJust (SP.relFileToPosix $ SP.fromPathRelFileW [PW.relfile|test\file|])
                `shouldBe` expectedPosixPath
            fromJust (SP.relFileToPosix $ SP.fromPathRelFileP [PP.relfile|test/file|])
                `shouldBe` expectedPosixPath
            fromJust (SP.relFileToPosix $ SP.fromPathRelFile [P.relfile|test/file|])
                `shouldBe` expectedPosixPath

    describe "extractRelPathPrefix correctly extracts prefix from rel path." $ do
        it "when path starts with multiple ../" $ do
            SP.extractRelPathPrefix "../../" `shouldBe` (SP.ParentDir 2, "")
            SP.extractRelPathPrefix "../../../a/b" `shouldBe` (SP.ParentDir 3, "a/b")
            -- TODO: Does not pass when running on Posix, since it does not care about Windows separator. Should it?
            SP.extractRelPathPrefix "..\\a\\b" `shouldBe` (SP.ParentDir 1, "a\\b")
        it "when path does not start with ../" $ do
            SP.extractRelPathPrefix "a/b" `shouldBe` (SP.NoPrefix, "a/b")
            SP.extractRelPathPrefix "b" `shouldBe` (SP.NoPrefix, "b")
            SP.extractRelPathPrefix "." `shouldBe` (SP.NoPrefix, ".")

    describe "Parsing from FilePath" $ do
        let runTest fpTrans parser fp = it (fp ++ " should parse into " ++ fpTrans fp) $
                (SP.toFilePath $ fromJust $ parser fp) `shouldBe` fpTrans fp
        describe "into standard System" $ do
            describe "into base Rel" $ do
                describe "captures one or multiple ../ at start of relative path" $ do
                    let test = runTest id
                    test SP.parseRelDir  (posixToSystemFp "../../a/b/")
                    test SP.parseRelDir  (posixToSystemFp "../../")
                    test SP.parseRelDir  (posixToSystemFp "./")
                    test SP.parseRelFile (posixToSystemFp "../a/b.txt")
                describe "can parse from system FilePath" $ do
                    let test = runTest id
                    test SP.parseRelDir  (posixToSystemFp "../a/b/")
                    test SP.parseRelDir  (posixToSystemFp "a/b/")
                    test SP.parseRelFile (posixToSystemFp "../a/b.txt")
                    test SP.parseRelFile (posixToSystemFp "a/b.txt")
                describe "can parse from posix FilePath" $ do
                    let test = runTest posixToSystemFp
                    test SP.parseRelDir  "../a/b/"
                    test SP.parseRelDir  "a/b/"
                    test SP.parseRelFile "../a/b.txt"
                    test SP.parseRelFile "a/b.txt"
            describe "into base Abs" $ do
                describe "can parse from system FilePath" $ do
                    let test = runTest id
                    test SP.parseAbsDir  (systemFpRoot FP.</> posixToSystemFp "a/b/")
                    test SP.parseAbsFile (systemFpRoot FP.</> posixToSystemFp "a/b.txt")

        describe "into standard Windows" $ do
            describe "into base Rel" $ do
                describe "captures one or multiple ../ at start of relative path" $ do
                    let test = runTest posixToWindowsFp
                    test SP.parseRelDirW  (posixToSystemFp "../../a/b/")
                    test SP.parseRelFileW (posixToSystemFp "../a/b.txt")
                describe "can parse from windows FilePath" $ do
                    let test = runTest id
                    test SP.parseRelDirW  "..\\a\\b\\"
                    test SP.parseRelDirW  "a\\b\\"
                    test SP.parseRelFileW "..\\a\\b.txt"
                    test SP.parseRelFileW "..\\..\\a\\b.txt" -- TODO: This one and one above are suspicious, shouldn't they fail when running on Linux because of how extractRelPathPrefix is implemented (on Linux it does not count windows path separator as valid separator)? I am suspecting that it somehow puts the whole thing into Path, which I would be surprised with because I thought that Path does not tolerate ..\\ at start of its path. Unless the fact that we are running this on Linux somehow messes up its PW.parseRelFile so it ignores windows separators and .. in front of them?
                    test SP.parseRelFileW "a\\b.txt"
                describe "can parse from posix FilePath" $ do
                    let test = runTest posixToWindowsFp
                    test SP.parseRelDirW  "../a/b/"
                    test SP.parseRelDirW  "a/b/"
                    test SP.parseRelFileW "../a/b.txt"
                    test SP.parseRelFileW "a/b.txt"
            describe "into base Abs" $ do
                describe "can parse from windows FilePath" $ do
                    let test = runTest id
                    test SP.parseAbsDirW  "C:\\a\\b\\"
                    test SP.parseAbsFileW "C:\\a\\b.txt"

        describe "into standard Posix" $ do
            describe "into base Rel" $ do
                describe "captures one or multiple ../ at start of relative path" $ do
                    let test = runTest id
                    test SP.parseRelDirP  "../../a/b/"
                    test SP.parseRelFileP "../a/b.txt"
                describe "can parse from posix FilePath" $ do
                    let test = runTest id
                    test SP.parseRelDirP  "../a/b/"
                    test SP.parseRelDirP  "a/b/"
                    test SP.parseRelFileP "../a/b.txt"
                    test SP.parseRelFileP "a/b.txt"
            describe "into base Abs" $ do
                describe "can parse from posix FilePath" $ do
                    let test = runTest id
                    test SP.parseAbsDirP  "/a/b/"
                    test SP.parseAbsFileP "/a/b.txt"
