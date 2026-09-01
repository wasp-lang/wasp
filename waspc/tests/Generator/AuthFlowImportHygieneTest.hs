module Generator.AuthFlowImportHygieneTest where

import Control.Monad (forM)
import Data.List (isInfixOf, isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | The import-hygiene guard over wasp-auth's flow templates.
--
-- After the restatement onto the runtime facets, wasp-auth's flows hold no
-- powers an adapter package cannot request: sessions through `wasp-sessions`,
-- identities through namespace facets, email through `email-send`. This test
-- is the review chokepoint that keeps it that way -- a flow template reaching
-- for a privileged module again fails here, so privilege growth is a
-- deliberate diff on this list, never an accident of imports.
--
-- Deliberately a denylist of privileged modules, not an allowlist of
-- everything: flows legitimately import wasp-auth's own unprivileged helpers
-- (validation, password hashing, jwt, express) and those should not need
-- test churn.
spec_AuthFlowImportHygiene :: Spec
spec_AuthFlowImportHygiene =
  describe "wasp-auth flow templates" $
    it "hold no privileged imports outside the runtime facets" $ do
      files <- listTsFilesRecursively waspAuthFlowTemplatesDir
      violations <-
        concat
          <$> forM
            files
            ( \file -> do
                contents <- readFile file
                return
                  [ file ++ " imports the privileged module " ++ show forbidden
                    | forbidden <- forbiddenImportSubstrings,
                      forbidden `isInfixOf` contents
                  ]
            )
      violations `shouldBe` []
  where
    waspAuthFlowTemplatesDir = "data/Generator/templates/server/src/auth/providers"
    forbiddenImportSubstrings =
      [ -- Direct session minting/revocation: only the `wasp-sessions` facet may.
        "from 'wasp/server/auth/session'",
        "sessionStore",
        "lucia",
        -- Raw identity storage: only the identity facets may.
        "identityStore",
        -- The app's email sender: only the `email-send` facet may.
        "emailSender",
        -- Raw database access.
        "from 'wasp/server/dbClient'"
      ]

listTsFilesRecursively :: FilePath -> IO [FilePath]
listTsFilesRecursively dir = do
  entries <- listDirectory dir
  concat
    <$> forM
      entries
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listTsFilesRecursively path
            else return [path | ".ts" `isSuffixOf` path]
      )
