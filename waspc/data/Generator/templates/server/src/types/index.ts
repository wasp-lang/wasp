{{={= =}=}}

import { type Application } from 'express'
import { Server } from 'http'

export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

export type ServerSetupFnContext = {
  app: Application,
  server: Server,
}

export type { Application } from 'express'
export type { Server } from 'http'

{=# isEmailAuthEnabled =}
export type { GetVerificationEmailContentFn, GetPasswordResetEmailContentFn } from '../auth/providers/email/types';
{=/ isEmailAuthEnabled =}
