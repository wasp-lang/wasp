module AI.GenerateNewProject.PageComponentFileTest where

import qualified Data.Map as M
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import Wasp.AI.GenerateNewProject.PageComponentFile

spec_PageComponentFileTest :: Spec
spec_PageComponentFileTest = do
  describe "getPageComponentFileContentWithFixedImports" $ do
    let mockAllPossibleWaspClientImports =
          M.fromList $
            [ ("useQuery", "import { useQuery } from '@wasp/queries';"),
              ("useAction", "import { useAction } from '@wasp/actions';"),
              ("useAuth", "import useAuth from '@wasp/auth/useAuth';"),
              ("someAction", "import someAction from '@wasp/actions/someAction';"),
              ("someQuery", "import someQuery from '@wasp/queries/someQuery';")
            ]
    it "should fix incorrect @wasp imports while keeping non-@wasp imports and removing made up @wasp ones." $ do
      let mockPageComponentFileContent =
            [trimming|
              import React from 'react';
              import { useAuth } from '@wasp/authorization/useAuth';
              import { useQuery, useAction } from '@wasp/queries_and_actions';
              import { Link } from 'react-router';
              import madeUpThingy from '@wasp/madeup';
              import { someAction } from '@wasp/actions';

              function HomePage () {
                ...
              }
            |]
      getPageComponentFileContentWithFixedImports
        mockPageComponentFileContent
        mockAllPossibleWaspClientImports
        `shouldBe` [trimming|
              import React from 'react';
              import { Link } from 'react-router';
              import useAuth from '@wasp/auth/useAuth';
              import { useQuery } from '@wasp/queries';
              import { useAction } from '@wasp/actions';
              import someAction from '@wasp/actions/someAction';

              function HomePage () {
                ...
              }
            |]

  describe "getImportedNameFromImport" $ do
    it "should correctly pick up names from various imports statements" $ do
      getImportedNamesFromImport "import { foo, barBar} from 'some/path'"
        `shouldBe` ["foo", "barBar"]
      getImportedNamesFromImport " import  fooFoo from \"../\" "
        `shouldBe` ["fooFoo"]

  describe "partitionComponentFileByImports" $ do
    it "should partition file content to wasp imports, non-wasp imports, and the rest of the file." $ do
      let fileContent =
            "\n"
              <> [trimming|
              import React from 'react';
              import { useAuth } from '@wasp/useAuth';
                import { useQuery } from '@wasp/queries';

              import { someAction } from '@wasp/actions';
              import { Link } from 'react-router';

              function importStuff () {
                importStuffNow("@wasp");
              }

              const a = 5;
           |]
              <> "\n\n"

      partitionComponentFileByImports fileContent
        `shouldBe` ( [ "import { useAuth } from '@wasp/useAuth';",
                       "import { useQuery } from '@wasp/queries';",
                       "import { someAction } from '@wasp/actions';"
                     ],
                     [ "import React from 'react';",
                       "import { Link } from 'react-router';"
                     ],
                     [ "",
                       "function importStuff () {",
                       "  importStuffNow(\"@wasp\");",
                       "}",
                       "",
                       "const a = 5;",
                       ""
                     ]
                   )
