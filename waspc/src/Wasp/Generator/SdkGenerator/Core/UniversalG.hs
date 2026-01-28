module Wasp.Generator.SdkGenerator.Core.UniversalG
  ( genUniversal,
  )
where

import StrongPath (relfile)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genUniversal :: Generator [FileDraft]
genUniversal =
  return
    [ mkTmplFd [relfile|universal/url.ts|],
      mkTmplFd [relfile|universal/types.ts|],
      mkTmplFd [relfile|universal/validators.ts|],
      mkTmplFd [relfile|universal/predicates.ts|],
      mkTmplFd [relfile|universal/ansiColors.ts|]
    ]
