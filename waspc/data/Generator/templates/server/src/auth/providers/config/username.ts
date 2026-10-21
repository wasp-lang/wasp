{{={= =}=}}
import { createUsernameAuthProvider } from "@wasp.sh/lib-auth/node";
import type { AuthHookTypes } from "wasp/server/auth";
import { sessions } from "wasp/server/auth/session";
import { authIdentities } from "wasp/server/auth/utils";
import * as hooks from "../../hooks.js";

{=# userSignupFields.isDefined =}
{=& userSignupFields.importStatement =}
const _waspUserSignupFields = {= userSignupFields.importIdentifier =}
{=/ userSignupFields.isDefined =}
{=^ userSignupFields.isDefined =}
const _waspUserSignupFields = undefined
{=/ userSignupFields.isDefined =}

const config = createUsernameAuthProvider<AuthHookTypes>(
  {
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    userSignupFields: _waspUserSignupFields,
  },
  {
    identities: authIdentities,
    sessions,
    hooks,
  },
);

export default config;
