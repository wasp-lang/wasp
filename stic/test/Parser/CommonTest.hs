module Parser.CommonTest where

import Test.Tasty.Hspec

import Text.Parsec
import Data.Either

import Lexer
import Parser.Common

spec_parseWaspCommon :: Spec
spec_parseWaspCommon = do
    describe "Parsing wasp element name and properties" $ do
        let parseWaspElementNameAndClosure elemKeyword p input =
                runWaspParser (waspElementNameAndClosure elemKeyword p) input

        it "When given valid wasp element declaration along with whitespace parser,\
            \ returns an expected result" $ do
            parseWaspElementNameAndClosure "app" whiteSpace "app someApp { }"
                `shouldBe` Right ("someApp", ())

        it "When given valid wasp element declaration along with char parser, returns\
            \ an expected result" $ do
            parseWaspElementNameAndClosure "app" (char 'a') "app someApp {a}"
                `shouldBe` Right ("someApp", 'a')

        it "When given wasp element declaration with invalid name, returns Left" $ do
            (isLeft $ parseWaspElementNameAndClosure "app" whiteSpace "app 1someApp { }")
                `shouldBe` True

    describe "Parsing wasp property - string literal" $ do
        let parseWaspPropertyStringLiteral key input =
                runWaspParser (waspPropertyStringLiteral key) input

        it "When given key/value with int value, returns Left." $ do
            isLeft (parseWaspPropertyStringLiteral "title" "title: 23")
                `shouldBe` True

        it "When given key/value with string value, returns a parsed value." $ do
            let appTitle = "my first app"
            parseWaspPropertyStringLiteral "title" ("title: \"" ++ appTitle ++ "\"")
                `shouldBe` Right appTitle

    describe "Parsing wasp property - closure {...}" $ do
        let parseWaspPropertyClosure key input =
                runWaspParser (waspPropertyClosure key) input

        it "When given int, returns Left." $ do
            isLeft (parseWaspPropertyClosure "content" "title: 23")
                `shouldBe` True

        it "When given content within braces, returns that content." $ do
            parseWaspPropertyClosure "content" "content: {  some content   }"
                `shouldBe` Right "some content"

    describe "Parsing wasp closure" $ do
        let parseWaspClosure input = runWaspParser waspClosure input
        let closureContent = "<div>hello world</div>"

        it "Returns the content of closure" $ do
            parseWaspClosure ("{ " ++ closureContent ++ " }")
                `shouldBe` Right closureContent

        it "Removes leading and trailing spaces" $ do
            parseWaspClosure ("{   " ++  closureContent ++ "   }")
                `shouldBe` Right closureContent
