{{={= =}=}}

import { setupOAuthRouter } from "../oauth/setupRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import { ProviderConfig, OAuthConfig } from "../types.js";

const oAuthConfig: OAuthConfig = {
    {=# oAuthConfigProps =}
    {= key =}: {=& value =},
    {=/ oAuthConfigProps =}
};

const config: ProviderConfig = {
    slug: "{= slug =}",
    init: makeOAuthInit({
        npmPackage: '{= npmPackage =}',
        passportImportPath: '{= passportConfigImport =}',
        oAuthConfig,
    }),
    setupRouter: setupOAuthRouter,
}

export default config;
