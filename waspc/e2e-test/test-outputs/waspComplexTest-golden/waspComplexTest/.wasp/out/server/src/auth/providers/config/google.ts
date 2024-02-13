
import { createRouter } from "../oauth/createRouter.js";
import { makeOAuthInit } from "../oauth/init.js";

import type { ProviderConfig } from "wasp/auth/providers/types";
import type { OAuthConfig } from "../oauth/types.js";

const _waspUserSignupFields = undefined
const _waspUserDefinedConfigFn = undefined

const _waspOAuthConfig: OAuthConfig = {
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_CLIENT_SECRET,
    scope: ['profile'],
};

const _waspConfig: ProviderConfig = {
    id: "google",
    displayName: "Google",
    init: makeOAuthInit({
        npmPackage: 'passport-google-oauth20',
        userSignupFields: _waspUserSignupFields,
        userDefinedConfigFn: _waspUserDefinedConfigFn,
        oAuthConfig: _waspOAuthConfig,
    }),
    createRouter,
}

export default _waspConfig;
