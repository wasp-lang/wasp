{{={= =}=}}
import { createAuthProvidersRouter } from "@wasp.sh/lib-auth/node";
{=# isExternalAuthEnabled =}
import { setupOneTimeCodeRoute } from "@wasp.sh/lib-auth/node";
import { exchangeCodeForTokenPath, tokenStore } from "wasp/server/auth";
import { sessions } from "wasp/server/auth/session";
import { authIdentities } from "wasp/server/auth/utils";
{=/ isExternalAuthEnabled =}

{=# providers =}
{=& importStatement =}
{=/ providers =}

const providers = [
  {=# providers =}
  {= importIdentifier =},
  {=/ providers =}
];

{=# isExternalAuthEnabled =}
const router = createAuthProvidersRouter(providers, (router) => {
  // Setting up a common route for all OAuth providers to exchange
  // one-time code for a session.
  setupOneTimeCodeRoute(router, {
    exchangeCodeForTokenPath,
    tokenStore,
    identities: authIdentities,
    sessions,
  });
});
{=/ isExternalAuthEnabled =}
{=^ isExternalAuthEnabled =}
const router = createAuthProvidersRouter(providers);
{=/ isExternalAuthEnabled =}

export default router;
