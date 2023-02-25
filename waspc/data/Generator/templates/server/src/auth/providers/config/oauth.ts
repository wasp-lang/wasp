{{={= =}=}}

import { setupOAuthRouter } from "../oauth/setupRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import { ProviderConfig } from "../types.js";

const config: ProviderConfig = {
    slug: "{= slug =}",
    init: makeOAuthInit({
        npmPackage: '{= npmPackage =}',
        passportImportPath: '{= passportConfigImport =}',
    }),
    setupRouter: setupOAuthRouter,
}

export default config;
