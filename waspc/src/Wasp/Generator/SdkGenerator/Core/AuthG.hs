module Wasp.Generator.SdkGenerator.Core.AuthG
  ( genAuth,
  )
where

import StrongPath (relfile)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genAuth :: Generator [FileDraft]
genAuth =
  return
    [ mkTmplFd [relfile|auth/password.ts|],
      mkTmplFd [relfile|auth/forms/internal/util.ts|],
      mkTmplFd [relfile|auth/forms/internal/auth-styles.css|],
      mkTmplFd [relfile|auth/forms/internal/Form.tsx|],
      mkTmplFd [relfile|auth/forms/internal/Form.module.css|],
      mkTmplFd [relfile|auth/forms/internal/Message.tsx|],
      mkTmplFd [relfile|auth/forms/internal/Message.module.css|]
    ]
