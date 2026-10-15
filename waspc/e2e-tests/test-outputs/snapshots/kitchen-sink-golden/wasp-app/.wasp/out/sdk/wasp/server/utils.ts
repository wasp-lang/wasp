
export { defineHandler, redirect } from '@wasp.sh/lib-sdk-core/node'

export { sleep } from '@wasp.sh/lib-sdk-core'

import type { AuthUserData } from '../auth/user.js'

// This is explicitly how Express expects extensions to their
// Request and Response objects to be done.
// https://github.com/DefinitelyTyped/DefinitelyTyped/blob/5d29b9be383902b0399f26072b4590ac61ca72c5/types/express-serve-static-core/index.d.ts#L6-L15
declare global {
  namespace Express {
    interface Request {
      user?: AuthUserData | null;
      sessionId?: string | null;
    }
  }
}
