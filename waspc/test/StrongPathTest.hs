module StrongPathTest where

import           Test.Tasty.Hspec

import           Data.Maybe              (fromJust)
import qualified Path                    as P
import qualified Path.Posix              as PP
import qualified Path.Windows            as PW
import qualified System.FilePath         as FP
import qualified System.FilePath.Posix   as FPP
import qualified System.FilePath.Windows as FPW

import           Fixtures                (systemFpRoot, systemPathRoot)
import           StrongPath
import           Test.Util               (posixToSystemFp, posixToWindowsFp)


data Bar
data Fizz

-- TODO: I should look into using QuickCheck to simplify / enhcance StrongPath tests,
--   it would probably be a good fit for some cases.

spec_StrongPath :: Spec
spec_StrongPath = do
    describe "Example with Foo file and Bar, Fizz and Kokolo dirs" $ do
        let fooFileInBarDir = fromPathRelFile [P.relfile|foo.txt|] :: Path (Rel Bar) File
        let barDirInFizzDir = fromPathRelDir [P.reldir|kokolo/bar|] :: Path (Rel Fizz) (Dir Bar)
        let fizzDir = (fromPathAbsDir $ systemPathRoot P.</> [P.reldir|fizz|]) :: Path Abs (Dir Fizz)
        let fooFile = (fizzDir </> barDirInFizzDir </> fooFileInBarDir) :: Path Abs File
        let fooFileInFizzDir = (barDirInFizzDir </> fooFileInBarDir) :: Path (Rel Fizz) File

        it "Paths are correctly concatenated" $ do
            P.toFilePath (toPathAbsFile fooFile) `shouldBe` posixToSystemFp "/fizz/kokolo/bar/foo.txt"
            P.toFilePath (toPathRelFile fooFileInFizzDir) `shouldBe` posixToSystemFp "kokolo/bar/foo.txt"

    it "Paths are unchanged when packed from Path.Path and unpacked to Path.Path" $ do
        let test pack unpack path = unpack (pack path) == path `shouldBe` True
        test fromPathRelFile toPathRelFile [P.relfile|some/file.txt|]
        test fromPathRelDir  toPathRelDir  [P.reldir|some/dir/|]
        test fromPathAbsFile toPathAbsFile $ systemPathRoot P.</> [P.relfile|some/file.txt|]
        test fromPathAbsDir  toPathAbsDir  $ systemPathRoot P.</> [P.reldir|some/file.txt|]

    describe "relDirToPosix/relFileToPosix correctly converts relative strong path to Posix" $ do
        describe "when strong path is relative dir" $ do
            let expectedPosixPath = fromPathRelDirP [PP.reldir|test/dir/|]
            it "from standard Win" $
                fromJust (relDirToPosix $ fromPathRelDirW [PW.reldir|test\dir\|])
                    `shouldBe` expectedPosixPath
            it "from standard Posix" $
                fromJust (relDirToPosix $ fromPathRelDirP [PP.reldir|test/dir/|])
                    `shouldBe` expectedPosixPath
            it "from standard System" $
                fromJust (relDirToPosix $ fromPathRelDir [P.reldir|test/dir/|])
                    `shouldBe` expectedPosixPath
        describe "correctly when strong path is relative file" $ do
            let expectedPosixPath = fromPathRelFileP [PP.relfile|test/file|]
            it "from standard Win" $
                fromJust (relFileToPosix $ fromPathRelFileW [PW.relfile|test\file|])
                    `shouldBe` expectedPosixPath
            it "from standard Posix" $
                fromJust (relFileToPosix $ fromPathRelFileP [PP.relfile|test/file|])
                    `shouldBe` expectedPosixPath
            it "from standard System" $
                fromJust (relFileToPosix $ fromPathRelFile [P.relfile|test/file|])
                    `shouldBe` expectedPosixPath

    describe "extractRelPathPrefix correctly extracts prefix from rel FilePath." $ do
        it "when path starts with multiple ../" $ do
            extractRelPathPrefix [FPP.pathSeparator] "../../" `shouldBe` (ParentDir 2, "")
            extractRelPathPrefix [FPP.pathSeparator] "../.." `shouldBe` (ParentDir 2, "")
            extractRelPathPrefix [FP.pathSeparator] ".." `shouldBe` (ParentDir 1, "")
            extractRelPathPrefix [FP.pathSeparator, FPP.pathSeparator]"../../../a/b" `shouldBe` (ParentDir 3, "a/b")
            extractRelPathPrefix [FPW.pathSeparator]"..\\a\\b" `shouldBe` (ParentDir 1, "a\\b")
        it "when path does not start with ../" $ do
            extractRelPathPrefix [FPP.pathSeparator] "a/b" `shouldBe` (NoPrefix, "a/b")
            extractRelPathPrefix [FP.pathSeparator] "b" `shouldBe` (NoPrefix, "b")
            extractRelPathPrefix [FP.pathSeparator] "." `shouldBe` (NoPrefix, ".")

    describe "Parsing from FilePath" $ do
        let runTest fpToParseIntoExpectedFp parser fpToParse =
                let expectedFp = fpToParseIntoExpectedFp fpToParse
                in it (fpToParse ++ " should parse into " ++ expectedFp) $ do
                    let sp = fromJust $ parser fpToParse
                    toFilePath sp `shouldBe` expectedFp
        let runTestRel fpToParseIntoExpectedFp parser fpToParse expectedNumParentDirs =
                let expectedFp = fpToParseIntoExpectedFp fpToParse
                in it (fpToParse ++ " should parse into " ++ expectedFp) $ do
                    let sp = fromJust $ parser fpToParse
                    toFilePath sp `shouldBe` expectedFp
                    relPathNumParentDirs sp `shouldBe` expectedNumParentDirs

        describe "into standard System" $ do
            describe "into base Rel" $ do
                describe "captures one or multiple ../ at start of relative path" $ do
                    let test = runTestRel id
                    test parseRelDir (posixToSystemFp "../../a/b/") 2
                    test parseRelDir  (posixToSystemFp "../") 1
                    test parseRelDir  (posixToSystemFp "../../") 2
                    test parseRelDir  (posixToSystemFp "./") 0
                    test parseRelFile (posixToSystemFp "../a/b.txt") 1
                describe "can parse from system FilePath" $ do
                    let test = runTestRel id
                    test parseRelDir  (posixToSystemFp "../a/b/") 1
                    test parseRelDir  (posixToSystemFp "a/b/") 0
                    test parseRelFile (posixToSystemFp "../a/b.txt") 1
                    test parseRelFile (posixToSystemFp "a/b.txt") 0
                describe "can parse from posix FilePath" $ do
                    let test = runTestRel posixToSystemFp
                    test parseRelDir  "../a/b/" 1
                    test parseRelDir  "a/b/" 0
                    test parseRelFile "../a/b.txt" 1
                    test parseRelFile "a/b.txt" 0
            describe "into base Abs" $ do
                describe "can parse from system FilePath" $ do
                    let test = runTest id
                    test parseAbsDir  (systemFpRoot FP.</> posixToSystemFp "a/b/")
                    test parseAbsFile (systemFpRoot FP.</> posixToSystemFp "a/b.txt")
                describe "can parse from FilePath with system root and posix separators" $ do
                    let test = runTest posixToSystemFp
                    test parseAbsDir  (systemFpRoot FP.</> "a/b/")
                    test parseAbsFile (systemFpRoot FP.</> "a/b.txt")

        describe "into standard Windows" $ do
            describe "into base Rel" $ do
                describe "captures one or multiple ../ at start of relative path" $ do
                    let test = runTestRel posixToWindowsFp
                    test parseRelDirW  (posixToSystemFp "../../a/b/") 2
                    test parseRelFileW (posixToSystemFp "../a/b.txt") 1
                describe "can parse from windows FilePath" $ do
                    let test = runTestRel id
                    test parseRelDirW  "..\\a\\b\\" 1
                    test parseRelDirW  "a\\b\\" 0
                    test parseRelFileW "..\\a\\b.txt" 1
                    test parseRelFileW "..\\..\\a\\b.txt" 2
                    test parseRelFileW "a\\b.txt" 0
                describe "can parse from posix FilePath" $ do
                    let test = runTestRel posixToWindowsFp
                    test parseRelDirW  "../a/b/" 1
                    test parseRelDirW  "a/b/" 0
                    test parseRelFileW "../a/b.txt" 1
                    test parseRelFileW "a/b.txt" 0
            describe "into base Abs" $ do
                describe "can parse from windows FilePath" $ do
                    let test = runTest id
                    test parseAbsDirW  "C:\\a\\b\\"
                    test parseAbsFileW "C:\\a\\b.txt"
                describe "can parse from FilePath with windows root and Posix separators" $ do
                    let test = runTest posixToWindowsFp
                    test parseAbsDirW  "C:\\a/b/"
                    test parseAbsFileW "C:\\a/b.txt"

        describe "into standard Posix" $ do
            describe "into base Rel" $ do
                describe "captures one or multiple ../ at start of relative path" $ do
                    let test = runTestRel id
                    test parseRelDirP  "../../a/b/" 2
                    test parseRelFileP "../a/b.txt" 1
                describe "can parse from posix FilePath" $ do
                    let test = runTestRel id
                    test parseRelDirP  "../a/b/" 1
                    test parseRelDirP  "a/b/" 0
                    test parseRelFileP "a/b.txt" 0
            describe "into base Abs" $ do
                describe "can parse from posix FilePath" $ do
                    let test = runTest id
                    test parseAbsDirP  "/a/b/"
                    test parseAbsFileP "/a/b.txt"

    describe "toFilePath correctly transforms strong path into FilePath" $ do
        let test msp efp = it ("toFilePath (" ++ show msp ++ ") = " ++ efp) $
                toFilePath (fromJust msp) `shouldBe` efp
        test (parseRelDir $ posixToSystemFp "../") (posixToSystemFp "../")
        test (parseRelDir $ posixToSystemFp "a/b") (posixToSystemFp "a/b/")
        test (parseRelFile $ posixToSystemFp "../../foo.txt") (posixToSystemFp "../../foo.txt")
        test (parseRelDirW "../") "..\\"
        test (parseRelDirP "../") "../"
        -- TODO: Add more tests.

    describe "`parent` correctly returns parent dir" $ do
        let test msp mexpectedSp =
                it ("parent (" ++ show msp ++ ") == " ++ show mexpectedSp) $ do
                    let sp = fromJust msp
                    let expectedSp = fromJust mexpectedSp
                    parent sp `shouldBe` expectedSp
        let tests relDirParser relFileParser absDirParser absFileParser root = do
                test (relDirParser "a/b") (relDirParser "a")
                test (relDirParser "../a") (relDirParser "..")
                test (relDirParser "..") (relDirParser "../..")
                test (relDirParser ".") (relDirParser "..")
                test (relFileParser "a.txt") (relDirParser ".")
                test (relFileParser "../a.txt") (relDirParser "..")
                test (relFileParser "a/b.txt") (relDirParser "a")
                test (absDirParser $ root ++ "a/b") (absDirParser $ root ++ "a")
                test (absDirParser root) (absDirParser root)
                test (absFileParser $ root ++ "a/b.txt") (absDirParser $ root ++ "a")
        describe "when standard is System" $
            tests parseRelDir parseRelFile parseAbsDir parseAbsFile systemFpRoot
        describe "when standard is Windows" $
            tests parseRelDirW parseRelFileW parseAbsDirW parseAbsFileW "C:\\"
        describe "when standard is Posix" $
            tests parseRelDirP parseRelFileP parseAbsDirP parseAbsFileP "/"

    describe "</> correctly concatenates two corresponding paths" $ do
        let test mlsp mrsp mexpectedSp =
                it (show mlsp ++ " </> " ++ show mrsp ++ " == " ++ show mexpectedSp) $ do
                    let lsp = fromJust mlsp
                    let rsp = fromJust mrsp
                    let expectedSp = fromJust mexpectedSp
                    (lsp </> rsp) `shouldBe` expectedSp
        let tests relDirParser relFileParser absDirParser absFileParser root = do
                test (relDirParser "a/b") (relFileParser "c.txt") (relFileParser "a/b/c.txt")
                test (relDirParser "a/b") (relFileParser "../c.txt") (relFileParser "a/c.txt")
                -- I can't figure out why are these test below failing! Path tests show that Path behaves as it should,
                -- and all the other parts also seem to behave ok, so what is causing the problem!?
                -- TODO: fails on Win, for Posix standard: expected: RelFileP "c.txt" (ParentDir 2) but got: RelFileP "./c.txt" (ParentDir 2)
                test (relDirParser "..") (relFileParser "../c.txt") (relFileParser "../../c.txt") 
                -- TODO: fails on Win, for Posix standard: expected: RelDirP "./" (ParentDir 2) but got: RelDirP "././" (ParentDir 2)
                test (relDirParser "..") (relDirParser "..") (relDirParser "../..")
                -- TODO: fails on Win, for Posix standard: expected: RelDirP "a/" (ParentDir 1) but got: RelDirP "./a/" (ParentDir 1)
                test (relDirParser ".") (relDirParser "../a") (relDirParser "../a")
                -- TODO: fails on Win, for Posix standard: expected: RelDirP "./" NoPrefix but got: RelDirP "././" NoPrefix
                test (relDirParser ".") (relDirParser ".") (relDirParser ".")
                test (relDirParser "a/b") (relDirParser "c/d") (relDirParser "a/b/c/d")
                test (relDirParser "../a/b") (relDirParser "c/d") (relDirParser "../a/b/c/d")
                test (absDirParser $ root ++ "a/b") (relFileParser "c.txt") (absFileParser $ root ++ "a/b/c.txt")
                test (absDirParser $ root ++ "a/b") (relFileParser "../c.txt") (absFileParser $ root ++ "a/c.txt")
                test (absDirParser $ root ++ "a") (relDirParser "../b") (absDirParser $ root ++ "b")
                -- TODO: fails on Win, for Posix standard: expected: AbsDirP "/" "but got: AbsDirP "/./"
                test (absDirParser $ root ++ "a/b") (relDirParser "../../../") (absDirParser root)
        describe "when standard is System" $
            tests parseRelDir parseRelFile parseAbsDir parseAbsFile systemFpRoot
        describe "when standard is Windows" $
            tests parseRelDirW parseRelFileW parseAbsDirW parseAbsFileW "C:\\"
        describe "when standard is Posix" $
            tests parseRelDirP parseRelFileP parseAbsDirP parseAbsFileP "/"


spec_Path :: Spec
spec_Path = do
    -- Just checking that Path behaves in a way that we expect it to behave.
    it "Path.Windows.parseRelDir correctly parses Windows path" $ do
        fromJust (PW.parseRelDir ".\\") `shouldBe` fromJust (PW.parseRelDir "./")
        fromJust (PW.parseRelDir "a\\\\b\\") `shouldBe` fromJust (PW.parseRelDir "a/b/")
        fromJust (PW.parseRelDir "a\\b") `shouldBe` fromJust (PW.parseRelDir "a/b")
        PW.toFilePath (fromJust $ PW.parseRelDir "a\\b\\") `shouldBe` "a\\b\\"

    describe "Concatenation of System . paths works as expected" $ do
        let test lp rp ep =
                it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
                    (lp P.</> rp) `shouldBe` ep
        test [P.reldir|.|] [P.reldir|.|] [P.reldir|.|]
        test [P.reldir|a|] [P.reldir|.|] [P.reldir|a|]
        test [P.reldir|.|] [P.reldir|a|] [P.reldir|a|]
        test [P.reldir|.|] [P.relfile|c.txt|] [P.relfile|c.txt|]

    -- describe "Concatenation of Win . paths works as expected" $ do
    --     let test lp rp ep =
    --             it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
    --                 (lp PW.</> rp) `shouldBe` ep
    --     -- TODO: Fails on Linux/Mac: expected: ".\\" but got: ".\\.\\"
    --     test [PW.reldir|.|] [PW.reldir|.|] [PW.reldir|.|]
    --     -- TODO: Fails on Linux/Mac: expected: "a\\" but got: ".\\a\\"
    --     test [PW.reldir|.|] [PW.reldir|a|] [PW.reldir|a|]
    --     -- TODO: Fails on Linux/Mac: expected: "a\\" but got: "a\\.\\"
    --     test [PW.reldir|a|] [PW.reldir|.|] [PW.reldir|a|]

    -- describe "Concatenation of Posix . paths works as expected" $ do
    --     let test lp rp ep =
    --             it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
    --                 (lp PP.</> rp) `shouldBe` ep
    --     -- TODO: Fails on Win: expected: "./" but got: "././"
    --     test [PP.reldir|.|] [PP.reldir|.|] [PP.reldir|.|]
    --     -- TODO: Fails on Win: expected: "a/" but got: "./a/"
    --     test [PP.reldir|.|] [PP.reldir|a|] [PP.reldir|a|]
    --     -- TODO: Fails on Win: expected: "a/" but got: "a/./"
    --     test [PP.reldir|a|] [PP.reldir|.|] [PP.reldir|a|]

    describe "Parsing rel path with .. at start should fail" $ do
        let test parser p =
                it (show p ++ " should successfully parse") $
                    parser p `shouldBe` Nothing
        describe "for PW.parseRelDir" $ do
            test PW.parseRelDir "../a"
            -- -- TODO: This fails on Linux/Mac! Weird, I thought Path does not allow relative paths starting with ..?
            -- --       expected: Nothing but got: Just "..\\a\\"
            -- test PW.parseRelDir "..\\a"
        describe "for P.parseRelDir" $ do
            test P.parseRelDir "../a"
            test P.parseRelDir $ ".." FP.</> "a"
        describe "for PP.parseRelDir" $ do
            test PP.parseRelDir "../a"
