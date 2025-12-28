module Wasp.Generator.SdkGenerator.Client.Common where

import StrongPath (Dir, Path', Rel, reldir)
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesProjectDir)

data ClientTemplatesDir

clientTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesProjectDir) (Dir ClientTemplatesDir)
clientTemplatesDirInSdkTemplatesDir = [reldir|client|]
