module Analyzer.ValidTest where

import Data.Either (fromRight, isRight)
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser hiding (withCtx)
import qualified Wasp.Analyzer.Parser as P
import Wasp.Analyzer.Parser.Valid (validateAst)
import qualified Wasp.Version as WV

spec_ValidateAst :: Spec
spec_ValidateAst = do
  it "Returns an error when entities are used" $ do
    validateAndParseSource (waspSourceLines ++ entityDeclarationLines)
      `shouldBe` Left
        ( "Entities can no longer be defined in the .wasp file. You should migrate your entities to the schema.prisma file. Read more: https://wasp.sh/docs/migrate-from-0-13-to-0-14#migrate-to-the-new-schemaprisma-file",
          P.Ctx
            ( P.SourceRegion
                (P.SourcePosition 34 1)
                (P.SourcePosition 37 5)
            )
        )

  it "Returns AST when everything is correct" $ do
    isRight (validateAndParseSource waspSourceLines) `shouldBe` True
  where
    validateAndParseSource = validateAst . parseSource

    parseSource = fromRight (error "Parsing went wrong") . parseStatements . unlines

    waspSourceLines =
      [ "app Todo {",
        "  wasp: {",
        "    version: \"^" ++ show WV.waspVersion ++ "\",",
        "  },",
        "  title: \"Todo App\",",
        "  head: [\"foo\", \"bar\"],",
        "  auth: {",
        "    userEntity: User,",
        "    methods: {",
        "      usernameAndPassword: {",
        "        userSignupFields: import { getUserFields } from \"@src/auth/signup.js\",",
        "      }",
        "    },",
        "    onAuthFailedRedirectTo: \"/\",",
        "  },",
        "}",
        "",
        "page HomePage {",
        "  component: import Home from \"@src/pages/Main\"",
        "}",
        "",
        "route HomeRoute { path: \"/\", to: HomePage }",
        "",
        "query getUsers {",
        "  fn: import { getAllUsers } from \"@src/foo.js\",",
        "  entities: [User]",
        "}",
        "",
        "action updateUser {",
        "  fn: import { updateUser } from \"@src/foo.js\",",
        "  entities: [User],",
        "  auth: true",
        "}"
      ]

    entityDeclarationLines =
      [ "entity User {=psl",
        "  id Int @id @default(autoincrement())",
        "  email String @unique",
        "psl=}"
      ]
