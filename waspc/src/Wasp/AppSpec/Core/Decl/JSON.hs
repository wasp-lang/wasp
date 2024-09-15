{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Core.Decl.JSON where

import Wasp.AppSpec.Core.Decl.JSON.TH (generateFromJsonInstanceForDecl)
import Wasp.AppSpec.Page ()

-- TODO: For which decls are we actually generating logic here? Is it only for imported ones? If so, we need import all of them here!
-- Is there a good way to ensure people don't forget to import them here?
-- One idea I have is to group IsDecl.hs code, Decl.hs code, and this JSON.hs all into a single Decl.hs file, with Decl/TH.hs next to it. and then that is where `instance IsDecl` are declared for any decls in AppSpec, in this central place. IsDecl is not even exported, to ensure it has to happen like this. And we would likely need Decl/Internal.hs that would contain Decl type + a few helper fuctions (stuff now in Decl.hs) so that Decl/TH.hs can avoid cyclic dependency with Decl.hs. Yeah, then Decl.hs and Decl/TH.hs would be importing from Decl/Internal.hs .
-- Yeah, i can confirm that instances need to be imported for TH to pick them up.

-- TODO: Implement FromJSON instance for all the declarations, for now I did it only for Page and Route. Should be straight forward.

$(generateFromJsonInstanceForDecl)
