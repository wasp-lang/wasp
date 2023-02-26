{{={= =}=}}

import { setupOAuthRouter } from "../oauth/setupRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import { ProviderConfig, OAuthConfig } from "../types.js";

{=# userFieldsFn.isDefined =}
{=& userFieldsFn.importStatement =}
{=/ userFieldsFn.isDefined =}
{=^ userFieldsFn.isDefined =}
import { getUserFieldsFn } from '../oauth/defaults.js'
{=/ userFieldsFn.isDefined =}
{=# configFn.isDefined =}
{=& configFn.importStatement =}
{=/ configFn.isDefined =}

{=# userFieldsFn.isDefined =}
const getUserFieldsFn = {= userFieldsFn.importIdentifier =}
{=/ userFieldsFn.isDefined =}
{=# configFn.isDefined =}
const userDefinedConfigFn = {= configFn.importIdentifier =}
{=/ configFn.isDefined =}
{=^ configFn.isDefined =}
const userDefinedConfigFn = undefined
{=/ configFn.isDefined =}

const oAuthConfig: OAuthConfig = {
    {=# oAuthConfigProps =}
    {= key =}: {=& value =},
    {=/ oAuthConfigProps =}
};

const config: ProviderConfig = {
    slug: "{= slug =}",
    name: "{= name =}",
    init: makeOAuthInit({
        npmPackage: '{= npmPackage =}',
        getUserFieldsFn,
        userDefinedConfigFn,
        oAuthConfig,
    }),
    setupRouter: setupOAuthRouter,
}

export default config;
