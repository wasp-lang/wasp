module Analyzer.ParserTest where

import Test.Tasty.Hspec
import Analyzer.Syntax
import Analyzer.Parser

-- Source and AST that covers all of the syntax features
source_1 :: String 
source_1 = unlines [ "app Todo {"
                   , "  title: \"Todo App\""
                   , "}"
                   , "page Home {"
                   , "  component: import HomePage from \"@ext/Home.jsx\","
                   , "  params: []"
                   , "}"
                   , "page Post {"
                   , "  component: import { PostPage } from \"@ext/Pages.jsx\","
                   , "  authRequired: true,"
                   , "  params: [{ name: \"post_id\", paramType: StringParam }, { name: \"version\", paramType: NumberParam }]"
                   , "}"
                   , "route HomeR {"
                   , "  path: \"/\","
                   , "  page: Home"
                   , "}"
                   , "entity PostEntity {=psl"
                   , "  id Int @id"
                   , "  content String"
                   , "psl=}"
                   , "dependencies Dependencies {=json"
                   , "  \"key\": \"value\","
                   , "json=}"
                   ]

ast_1 :: AST
ast_1 = AST [ Decl "app" "Todo" $ Dict
                [ ("title", StringLiteral "Todo App") ]
            , Decl "page" "Home" $ Dict
                [ ("component", ExtImport (ExtImportModule "HomePage") "@ext/Home.jsx")
                , ("params", List [])
                ]
            , Decl "page" "Post" $ Dict
                [ ("component", ExtImport (ExtImportField "PostPage") "@ext/Pages.jsx")
                , ("authRequired", BoolLiteral True)
                , ("params"
                  , List [ Dict [ ("name", StringLiteral "post_id")
                                , ("paramType", Var "StringParam")
                                ]
                         , Dict [ ("name", StringLiteral "version")
                                , ("paramType", Var "NumberParam")
                                ]
                         ]
                  )
                ]
            , Decl "route" "HomeR" $ Dict
                [ ("path", StringLiteral "/")
                , ("page", Var "Home")
                ]
            , Decl "entity" "PostEntity" $ Quoter
                "psl"
                "\n  id Int @id\n  content String\n"
                "psl"
            , Decl "dependencies" "Dependencies" $ Quoter
                "json"
                "\n  \"key\": \"value\",\n"
                "json"
            ]

spec_Parser :: Spec
spec_Parser = do
  describe "Analyzer.Parser" $ do

    it "Parses input into the correct AST" $ do
      parse source_1 `shouldBe` Right ast_1

    it "Allows trailing commas in lists and dictionaries" $ do
      let source = unlines [ "test Decl {"
                           , "  list: [ 1, ],"
                           , "}"
                           ]
      let ast = AST [ Decl "test" "Decl" $ Dict
                        [ ("list", List [ IntegerLiteral 1 ] ) ]
                    ]
      parse source `shouldBe` Right ast
