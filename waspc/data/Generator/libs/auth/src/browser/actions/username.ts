import { handleApiError, initSession } from "@wasp.sh/lib-sdk-core/browser";
import type { KyInstance } from "ky";
import { SessionResponseSchema } from "../../shared/responseSchemas.js";

export type UsernameSignupData<ExtraSignupFields extends object> = {
  username: string;
  password: string;
} & ExtraSignupFields;

export type UsernameAuthActions<ExtraSignupFields extends object> = {
  login(data: { username: string; password: string }): Promise<void>;
  signup(data: UsernameSignupData<ExtraSignupFields>): Promise<void>;
};

export type UsernameAuthActionsConfig = {
  /** The app's API client. */
  api: KyInstance;
  /** Server paths of the username and password auth routes. */
  paths: {
    login: string;
    signup: string;
  };
};

/**
 * Creates the client functions of the username and password auth flow.
 *
 * `ExtraSignupFields` are the app's user signup fields, which the signup form
 * sends along with the username and password.
 */
export function createUsernameAuthActions<
  ExtraSignupFields extends object = {},
>({
  api,
  paths,
}: UsernameAuthActionsConfig): UsernameAuthActions<ExtraSignupFields> {
  async function login(data: {
    username: string;
    password: string;
  }): Promise<void> {
    try {
      const { sessionId } = await api
        .post(paths.login, {
          json: data,
        })
        .json(SessionResponseSchema);
      await initSession(sessionId);
    } catch (error) {
      throw handleApiError(error);
    }
  }

  async function signup(
    data: UsernameSignupData<ExtraSignupFields>,
  ): Promise<void> {
    try {
      await api.post(paths.signup, {
        json: data,
      });
    } catch (error) {
      throw handleApiError(error);
    }
  }

  return { login, signup };
}
