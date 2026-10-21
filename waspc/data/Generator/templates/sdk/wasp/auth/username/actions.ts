{{={= =}=}}
import type * as AuthProviders from "@wasp.sh/lib-auth/browser";
import { createUsernameAuthActions } from "@wasp.sh/lib-auth/browser";
import { api } from "../../api/index.js";
{=# isUsernameAndPasswordUserSignupFieldsDefined =}
import type { UserUsernameAndPasswordSignupFields } from "../providers";
{=/ isUsernameAndPasswordUserSignupFieldsDefined =}

{=# isUsernameAndPasswordUserSignupFieldsDefined =}
type ExtraSignupFields = UserUsernameAndPasswordSignupFields;
{=/ isUsernameAndPasswordUserSignupFieldsDefined =}
{=^ isUsernameAndPasswordUserSignupFieldsDefined =}
type ExtraSignupFields = {};
{=/ isUsernameAndPasswordUserSignupFieldsDefined =}

// PRIVATE API
export type UsernameSignupData =
  AuthProviders.UsernameSignupData<ExtraSignupFields>;

// PUBLIC API
export const { login, signup } = createUsernameAuthActions<ExtraSignupFields>({
  api,
  paths: {
    login: "{= loginPath =}",
    signup: "{= signupPath =}",
  },
});
