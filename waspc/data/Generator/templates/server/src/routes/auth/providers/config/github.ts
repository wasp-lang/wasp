import { setupOAuthRouter } from "../oauth/setupRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import { ProviderConfig, ProviderType } from "../types.js";

const config: ProviderConfig = {
    name: ProviderType.github,
    slug: "github",
    init: makeOAuthInit({
        npmPackage: 'passport-github2',
        passportImportPath: '../../passport/github/config.js',
    }),
    setupRouter: setupOAuthRouter,
}

export default config;
