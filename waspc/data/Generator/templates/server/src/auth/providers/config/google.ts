{{={= =}=}}

import { setupOAuthRouter } from "../oauth/setupRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import { ProviderConfig, ProviderType } from "../types.js";

const config: ProviderConfig = {
    name: ProviderType.google,
    slug: "{= slug =}",
    init: makeOAuthInit({
        npmPackage: '{= npmPackage =}',
        passportImportPath: '{= passportConfigImport =}',
    }),
    setupRouter: setupOAuthRouter,
}

export default config;
