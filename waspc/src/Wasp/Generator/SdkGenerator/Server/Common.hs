module Wasp.Generator.SdkGenerator.Server.Common where

import StrongPath
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesProjectDir)

data ServerTemplatesDir

serverTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesProjectDir) (Dir ServerTemplatesDir)
serverTemplatesDirInSdkTemplatesDir = [reldir|server|]
