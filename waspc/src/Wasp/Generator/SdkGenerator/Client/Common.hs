module Wasp.Generator.SdkGenerator.Client.Common where

import StrongPath
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesProjectDir)

data ClientTemplatesDir

clientTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesProjectDir) (Dir ClientTemplatesDir)
clientTemplatesDirInSdkTemplatesDir = [reldir|client|]
