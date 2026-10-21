import { createMicrosoftAuthProvider } from "@wasp.sh/lib-auth/node";
import { microsoft, type AuthHookTypes } from "wasp/server/auth";
import { oauthDependencies } from "../oauth/dependencies.js";

import { microsoftUserSignupFields } from "../../../../../../../src/features/auth/providers/microsoft"
const _waspUserSignupFields = microsoftUserSignupFields
import { microsoftConfig } from "../../../../../../../src/features/auth/providers/microsoft"
const _waspUserDefinedConfigFn = microsoftConfig

const _waspConfig = createMicrosoftAuthProvider<AuthHookTypes>(
  {
    provider: microsoft,
    requiredScopes: ["openid", "profile", "email"],
    userDefinedConfigFn: _waspUserDefinedConfigFn,
    userSignupFields: _waspUserSignupFields,
  },
  oauthDependencies,
);

export default _waspConfig;
