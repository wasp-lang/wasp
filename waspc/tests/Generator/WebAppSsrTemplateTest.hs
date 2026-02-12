module Generator.WebAppSsrTemplateTest where

import Data.Aeson (object, (.=))
import qualified Data.Text as T
import StrongPath (File, Path', Rel, relfile)
import Test.Hspec
import Wasp.Generator.Templates (TemplatesDir, compileAndRenderTemplate)

spec_WebAppSsrTemplate :: Spec
spec_WebAppSsrTemplate =
  describe "web-app/server-ssr.mjs template" $ do
    it "renders baseDir JSON string placeholder" $ do
      renderedTemplate <-
        compileAndRenderTemplate
          serverSsrTemplatePath
          (object ["baseDirJson" .= ("\"/\"" :: String)])

      renderedTemplate `shouldSatisfy` T.isInfixOf "const baseDir = \"/\";"
      renderedTemplate `shouldNotSatisfy` T.isInfixOf "{=& baseDirJson =}"

    it "does not HTML-escape baseDir content" $ do
      renderedTemplate <-
        compileAndRenderTemplate
          serverSsrTemplatePath
          (object ["baseDirJson" .= ("\"/foo&bar\"" :: String)])

      renderedTemplate `shouldSatisfy` T.isInfixOf "const baseDir = \"/foo&bar\";"
      renderedTemplate `shouldNotSatisfy` T.isInfixOf "const baseDir = \"/foo&amp;bar\";"
  where
    serverSsrTemplatePath :: Path' (Rel TemplatesDir) (File ())
    serverSsrTemplatePath = [relfile|web-app/server-ssr.mjs|]
