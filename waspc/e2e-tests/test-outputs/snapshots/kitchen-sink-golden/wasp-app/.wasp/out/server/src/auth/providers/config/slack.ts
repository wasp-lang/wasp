import { createSlackAuthProvider } from "@wasp.sh/lib-auth/node";
import { slack, type AuthHookTypes } from "wasp/server/auth";
import { oauthDependencies } from "../oauth/dependencies.js";

import { slackUserSignupFields } from "../../../../../../../src/features/auth/providers/slack"
const _waspUserSignupFields = slackUserSignupFields
import { slackConfig } from "../../../../../../../src/features/auth/providers/slack"
const _waspUserDefinedConfigFn = slackConfig

const _waspConfig = createSlackAuthProvider<AuthHookTypes>(
  {
    provider: slack,
    requiredScopes: ["openid"],
    userDefinedConfigFn: _waspUserDefinedConfigFn,
    userSignupFields: _waspUserSignupFields,
  },
  oauthDependencies,
);

export default _waspConfig;
