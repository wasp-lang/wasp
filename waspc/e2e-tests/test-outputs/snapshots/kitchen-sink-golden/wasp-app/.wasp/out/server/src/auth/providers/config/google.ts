import { createGoogleAuthProvider } from "@wasp.sh/lib-auth/node";
import { google, type AuthHookTypes } from "wasp/server/auth";
import { oauthDependencies } from "../oauth/dependencies.js";

import { googleUserSignupFields } from "../../../../../../../src/features/auth/providers/google"
const _waspUserSignupFields = googleUserSignupFields
import { googleConfig } from "../../../../../../../src/features/auth/providers/google"
const _waspUserDefinedConfigFn = googleConfig

const _waspConfig = createGoogleAuthProvider<AuthHookTypes>(
  {
    provider: google,
    requiredScopes: ["profile"],
    userDefinedConfigFn: _waspUserDefinedConfigFn,
    userSignupFields: _waspUserSignupFields,
  },
  oauthDependencies,
);

export default _waspConfig;
