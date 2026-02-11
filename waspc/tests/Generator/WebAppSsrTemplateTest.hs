module Generator.WebAppSsrTemplateTest where

import Data.Aeson (object, (.=))
import qualified Data.Text as T
import StrongPath (File, Path', Rel, relfile)
import Test.Hspec
import Wasp.Generator.Templates (TemplatesDir, compileAndRenderTemplate)

spec_WebAppSsrTemplate :: Spec
spec_WebAppSsrTemplate =
  describe "web-app/server-ssr.mjs template" $ do
    it "renders baseDir placeholder" $ do
      renderedTemplate <-
        compileAndRenderTemplate
          serverSsrTemplatePath
          (object ["baseDir" .= ("/" :: String)])

      renderedTemplate `shouldSatisfy` T.isInfixOf "const baseDir = \"/\";"
      renderedTemplate `shouldNotSatisfy` T.isInfixOf "{= baseDir =}"
  where
    serverSsrTemplatePath :: Path' (Rel TemplatesDir) (File ())
    serverSsrTemplatePath = [relfile|web-app/server-ssr.mjs|]
