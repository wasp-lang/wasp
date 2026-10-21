{{={= =}=}}
import { {= createProviderFunction =} } from "@wasp.sh/lib-auth/node";
import { {= providerId =}, type AuthHookTypes } from "wasp/server/auth";
{=# isKeycloak =}
import { env } from "wasp/server";
{=/ isKeycloak =}
import { oauthDependencies } from "../oauth/dependencies.js";

{=# userSignupFields.isDefined =}
{=& userSignupFields.importStatement =}
const _waspUserSignupFields = {= userSignupFields.importIdentifier =}
{=/ userSignupFields.isDefined =}
{=^ userSignupFields.isDefined =}
const _waspUserSignupFields = undefined
{=/ userSignupFields.isDefined =}
{=# configFn.isDefined =}
{=& configFn.importStatement =}
const _waspUserDefinedConfigFn = {= configFn.importIdentifier =}
{=/ configFn.isDefined =}
{=^ configFn.isDefined =}
const _waspUserDefinedConfigFn = undefined
{=/ configFn.isDefined =}

const _waspConfig = {= createProviderFunction =}<AuthHookTypes>(
  {
    provider: {= providerId =},
    requiredScopes: {=& requiredScopes =},
    userDefinedConfigFn: _waspUserDefinedConfigFn,
    userSignupFields: _waspUserSignupFields,
    {=# isKeycloak =}
    realmUrl: env.KEYCLOAK_REALM_URL,
    {=/ isKeycloak =}
  },
  oauthDependencies,
);

export default _waspConfig;
