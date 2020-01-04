module Parser.CommonTest where

import Test.Tasty.Hspec

import Text.Parsec
import Data.Either

import Lexer
import Parser.Common

spec_parseWaspCommon :: Spec
spec_parseWaspCommon = do
    describe "Parsing wasp element linked to an entity" $ do
        it "When given a valid declaration, parses it correctly." $ do
            runWaspParser (waspElementLinkedToEntity "entity-form" whiteSpace) "entity-form<Task> TaskForm { }"
                `shouldBe` Right ("Task", "TaskForm", ())

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

    describe "Parsing wasp closure" $ do
        it "Parses a closure with braces {}" $ do
            runWaspParser (waspClosure (symbol "content"))  "{ content }"
                `shouldBe` Right "content"
        
        it "Does not parse a closure with brackets []" $ do
            (isLeft $ runWaspParser (waspClosure (symbol "content")) "[ content ]")
                `shouldBe` True

    describe "Parsing wasp property with a closure as a value" $ do
        it "When given a string as a key and closure as a value, returns closure content." $ do
            runWaspParser (waspPropertyClosure "someKey" (symbol "content")) "someKey: { content }"
                `shouldBe` Right "content"

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

    describe "Parsing wasp property - jsx closure {=jsx...jsx=}" $ do
        let parseWaspPropertyJsxClosure key input =
                runWaspParser (waspPropertyJsxClosure key) input

        it "When given unexpected property key, returns Left." $ do
            isLeft (parseWaspPropertyJsxClosure "content" "title: 23")
                `shouldBe` True

        it "When given content within jsx closure, returns that content." $ do
            parseWaspPropertyJsxClosure "content" "content: {=jsx  some content   jsx=}"
                `shouldBe` Right "some content"

    describe "Parsing wasp jsx closure" $ do
        let parseWaspJsxClosure input = runWaspParser waspJsxClosure input
        let closureContent = "<div>hello world</div>"

        it "Returns the content of closure" $ do
            parseWaspJsxClosure ("{=jsx " ++ closureContent ++ " jsx=}")
                `shouldBe` Right closureContent

        it "Can parse braces {} within the closure" $ do
            let closureContentWithBraces = "<div>hello world {task.length}</div>"

            parseWaspJsxClosure ("{=jsx " ++ closureContentWithBraces ++ " jsx=}")
                `shouldBe` Right closureContentWithBraces

        it "Removes leading and trailing spaces" $ do
            parseWaspJsxClosure ("{=jsx   " ++  closureContent ++ "   jsx=}")
                `shouldBe` Right closureContent
