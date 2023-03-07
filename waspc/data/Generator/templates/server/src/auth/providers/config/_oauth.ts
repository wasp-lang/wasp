{{={= =}=}}

import { createRouter } from "../oauth/createRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import { ProviderConfig, OAuthConfig } from "../types.js";

{=# userFieldsFn.isDefined =}
{=& userFieldsFn.importStatement =}
const _waspGetUserFieldsFn = {= userFieldsFn.importIdentifier =}
{=/ userFieldsFn.isDefined =}
{=^ userFieldsFn.isDefined =}
import { getUserFieldsFn as _waspGetUserFieldsFn } from '../oauth/defaults.js'
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
