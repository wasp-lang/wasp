-- Needed by generateFromJsonInstanceForDecl (TH)
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wasp.AppSpec.Core.Decl.JSON where

import Wasp.AppSpec.Action ()
import Wasp.AppSpec.Api ()
import Wasp.AppSpec.ApiNamespace ()
import Wasp.AppSpec.App ()
import Wasp.AppSpec.Core.Decl.JSON.TH (generateFromJsonInstanceForDecl)
import Wasp.AppSpec.Crud ()
import Wasp.AppSpec.Entity ()
import Wasp.AppSpec.Job ()
import Wasp.AppSpec.Page ()
import Wasp.AppSpec.Query ()
import Wasp.AppSpec.Route ()

-- This TH function assumes that all IsDecl instances are imported in this file.
-- It needs this to be able to pick them up.
-- TODO: Is there a way to ensure we don't forget to import the instances of IsDecl here
-- as we add / remove them?
-- I tried centralizing all IsDecl instances themselves in this file, but failed to get
-- it working, mostly due to `Ref a` which requires `(IsDecl a) =>`.
$(generateFromJsonInstanceForDecl)
