{{={= =}=}}

import { createRouter } from "../oauth/createRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import type { ProviderConfig } from "../types.js";
import type { OAuthConfig } from "../oauth/types.js";

{=# userFieldsFn.isDefined =}
{=& userFieldsFn.importStatement =}
const _waspGetUserFieldsFn = {= userFieldsFn.importIdentifier =}
{=/ userFieldsFn.isDefined =}
{=^ userFieldsFn.isDefined =}
const _waspGetUserFieldsFn = undefined
{=/ userFieldsFn.isDefined =}
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
        getUserFieldsFn: _waspGetUserFieldsFn,
        userDefinedConfigFn: _waspUserDefinedConfigFn,
        oAuthConfig: _waspOAuthConfig,
    }),
    createRouter,
}

export default _waspConfig;
