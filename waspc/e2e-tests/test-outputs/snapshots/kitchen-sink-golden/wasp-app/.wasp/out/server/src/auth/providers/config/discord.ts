import { createDiscordAuthProvider } from "@wasp.sh/lib-auth/node";
import { discord, type AuthHookTypes } from "wasp/server/auth";
import { oauthDependencies } from "../oauth/dependencies.js";

import { discordUserSignupFields } from "../../../../../../../src/features/auth/providers/discord"
const _waspUserSignupFields = discordUserSignupFields
import { discordConfig } from "../../../../../../../src/features/auth/providers/discord"
const _waspUserDefinedConfigFn = discordConfig

const _waspConfig = createDiscordAuthProvider<AuthHookTypes>(
  {
    provider: discord,
    requiredScopes: ["identify"],
    userDefinedConfigFn: _waspUserDefinedConfigFn,
    userSignupFields: _waspUserSignupFields,
  },
  oauthDependencies,
);

export default _waspConfig;
