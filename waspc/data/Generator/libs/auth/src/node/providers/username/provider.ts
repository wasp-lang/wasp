import { Router } from "express";
import type { ProviderName } from "../../../shared/providerData.js";
import type { AuthHookTypes, AuthHooks } from "../../hooks.js";
import type { AuthIdentities, UserSignupFields } from "../../identities.js";
import type { ProviderConfig } from "../../providers.js";
import type { Sessions } from "../../sessions.js";
import { getLoginRoute } from "./login.js";
import { getSignupRoute } from "./signup.js";

/** What the username and password auth flows need from the framework and the app. */
export type UsernameAuthDependencies<H extends AuthHookTypes> = {
  identities: AuthIdentities<H["entities"]>;
  sessions: Sessions<unknown>;
  hooks: AuthHooks<H>;
};

export type UsernameAuthConfig = {
  id: ProviderName;
  displayName: string;
  userSignupFields: UserSignupFields | undefined;
};

export function createUsernameAuthProvider<H extends AuthHookTypes>(
  config: UsernameAuthConfig,
  dependencies: UsernameAuthDependencies<H>,
): ProviderConfig {
  return {
    id: config.id,
    displayName: config.displayName,
    createRouter() {
      const router = Router();

      router.post("/login", getLoginRoute(dependencies));
      const signupRoute = getSignupRoute(
        {
          userSignupFields: config.userSignupFields,
        },
        dependencies,
      );
      router.post("/signup", signupRoute);

      return router;
    },
  };
}
