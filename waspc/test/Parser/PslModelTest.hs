module Parser.PslModelTest where

import           Test.Tasty.Hspec

import           Data.Either      (isLeft)
import           Parser.Common    (runWaspParser)
import           Parser.PslModel  (model, body, attrArgument)
import qualified PslModelAst      as AST


spec_parsePslModel :: Spec
spec_parsePslModel = do
    describe "Complex example" $ do
        let pslModel = "model User {\n" ++ pslBody ++ "\n}"
            expectedModelAst = AST.Model "User" expectedBodyAst
            pslBody =
                unlines
                [ "  id Int @id @default(value: autoincrement())"
                , "  email String?"
                , "  posts Post[] @relation(\"UserPosts\", references: [id]) @customattr"
                , ""
                , "  @@someattr([id, email], 2 + 4, [posts])"
                ]
            expectedBodyAst =
                AST.Body
                [ AST.ElementField
                    ( AST.Field
                        { AST._name = "id"
                        , AST._type = AST.Int
                        , AST._typeModifiers = []
                        , AST._attrs =
                          [ AST.Attribute
                              { AST._attrName = "id"
                              , AST._attrArgs = []
                              }
                          , AST.Attribute
                              { AST._attrName = "default"
                              , AST._attrArgs =
                                [ AST.AttrArgNamed "value" (AST.AttrArgFunc "autoincrement")
                                ]
                              }
                          ]
                        }
                    )
                , AST.ElementField
                    ( AST.Field
                        { AST._name = "email"
                        , AST._type = AST.String
                        , AST._typeModifiers = [AST.Optional]
                        , AST._attrs = []
                        }
                    )
                , AST.ElementField
                    ( AST.Field
                        { AST._name = "posts"
                        , AST._type = AST.UserType "Post"
                        , AST._typeModifiers = [AST.List]
                        , AST._attrs =
                          [ AST.Attribute
                              { AST._attrName = "relation"
                              , AST._attrArgs =
                                [ AST.AttrArgUnnamed (AST.AttrArgString "UserPosts")
                                , AST.AttrArgNamed "references" (AST.AttrArgFieldRefList ["id"])
                                ]
                              }
                          , AST.Attribute
                              { AST._attrName = "customattr"
                              , AST._attrArgs = []
                              }
                          ]
                        }
                    )
                , AST.ElementBlockAttribute
                    ( AST.Attribute
                        { AST._attrName = "someattr"
                        , AST._attrArgs =
                          [ AST.AttrArgUnnamed (AST.AttrArgFieldRefList ["id", "email"])
                          , AST.AttrArgUnnamed (AST.AttrArgUnknown "2 + 4")
                          , AST.AttrArgUnnamed (AST.AttrArgFieldRefList ["posts"])
                          ]
                        }
                    )
                ]

        it "Body parser correctly parses" $ do
          runWaspParser body pslBody `shouldBe` Right expectedBodyAst

        it "Model parser correctly parses" $ do
          runWaspParser model pslModel `shouldBe` Right expectedModelAst

    describe "Body parser" $ do
      describe "Fails if input is not valid PSL" $ do
          let runTest psl = it psl $ isLeft (runWaspParser body psl) `shouldBe` True
          mapM_ runTest
              [ "  noType"
              , "  @startsWithAttribute"
              , "  @@@tooManyMonkeys"
              ]

    describe "Attribute argument parser" $ do
        let tests =
                [ ( "[foo, bar]"
                  , AST.AttrArgUnnamed (AST.AttrArgFieldRefList ["foo", "bar"])
                  )
                , ( "\"test\""
                  , AST.AttrArgUnnamed (AST.AttrArgString "test")
                  )
                , ( "foo: bar()"
                  , AST.AttrArgNamed "foo" (AST.AttrArgFunc "bar")
                  )
                , ( "Bob"
                  , AST.AttrArgUnnamed (AST.AttrArgIdentifier "Bob")
                  )
                , ( "42.3"
                  , AST.AttrArgUnnamed (AST.AttrArgNumber "42.3")
                  )
                , ( "2 + 3, ..."
                  , AST.AttrArgUnnamed (AST.AttrArgUnknown "2 + 3")
                  )

                ]
        let runTest (psl, expected) =
                it ("correctly parses " ++ psl) $ runWaspParser attrArgument psl `shouldBe` Right expected
        mapM_ runTest tests
