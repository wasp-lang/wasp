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

{=# isExternalAuthEnabled =}
export type { GetUserFieldsFn } from '../auth/providers/oauth/types';
{=/ isExternalAuthEnabled =}

{=# isEmailAuthEnabled =}
export { type GetVerificationEmailContentFn, type GetPasswordResetEmailContentFn, defineAdditionalSignupFields } from '../auth/providers/email/types.js';
{=/ isEmailAuthEnabled =}
{=# isLocalAuthEnabled =}
export { defineAdditionalSignupFields } from '../auth/providers/local/types.js';
{=/ isLocalAuthEnabled =}
