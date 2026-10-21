import { createGitHubAuthProvider } from "@wasp.sh/lib-auth/node";
import { github, type AuthHookTypes } from "wasp/server/auth";
import { oauthDependencies } from "../oauth/dependencies.js";

import { gitHubUserSignupFields } from "../../../../../../../src/features/auth/providers/github"
const _waspUserSignupFields = gitHubUserSignupFields
import { gitHubConfig } from "../../../../../../../src/features/auth/providers/github"
const _waspUserDefinedConfigFn = gitHubConfig

const _waspConfig = createGitHubAuthProvider<AuthHookTypes>(
  {
    provider: github,
    requiredScopes: [],
    userDefinedConfigFn: _waspUserDefinedConfigFn,
    userSignupFields: _waspUserSignupFields,
  },
  oauthDependencies,
);

export default _waspConfig;
