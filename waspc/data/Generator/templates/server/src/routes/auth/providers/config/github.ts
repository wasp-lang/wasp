{{={= =}=}}

import { setupOAuthRouter } from "../oauth/setupRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import { ProviderConfig, ProviderType } from "../types.js";

const config: ProviderConfig = {
    name: ProviderType.github,
    slug: "github",
    init: makeOAuthInit({
        npmPackage: '{= npmPackage =}',
        passportImportPath: '{= passportConfigImport =}',
    }),
    setupRouter: setupOAuthRouter,
}

export default config;
