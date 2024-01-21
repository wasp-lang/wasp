{{={= =}=}}

import { createRouter } from "../oauth/createRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import type { ProviderConfig } from "../types.js";
import type { OAuthConfig } from "../oauth/types.js";

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

const _waspOAuthConfig: OAuthConfig = {
    {=# oAuthConfigProps =}
    {= key =}: {=& value =},
    {=/ oAuthConfigProps =}
};

const _waspConfig: ProviderConfig = {
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    init: makeOAuthInit({
        npmPackage: '{= npmPackage =}',
        userSignupFields: _waspUserSignupFields,
        userDefinedConfigFn: _waspUserDefinedConfigFn,
        oAuthConfig: _waspOAuthConfig,
    }),
    createRouter,
}

export default _waspConfig;
