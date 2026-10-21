import { handleApiError, initSession } from "@wasp.sh/lib-sdk-core/browser";
import type { KyInstance } from "ky";
import {
  SessionResponseSchema,
  SuccessResponseSchema,
} from "../../shared/responseSchemas.js";

export type EmailSignupData<ExtraSignupFields extends object> = {
  email: string;
  password: string;
} & ExtraSignupFields;

export type EmailAuthActions<ExtraSignupFields extends object> = {
  login(data: { email: string; password: string }): Promise<void>;
  signup(
    data: EmailSignupData<ExtraSignupFields>,
  ): Promise<{ success: boolean }>;
  requestPasswordReset(data: { email: string }): Promise<{ success: boolean }>;
  resetPassword(data: {
    token: string;
    password: string;
  }): Promise<{ success: boolean }>;
  verifyEmail(data: {
    token: string;
  }): Promise<{ success: boolean; reason?: string }>;
};

export type EmailAuthActionsConfig = {
  /** The app's API client. */
  api: KyInstance;
  /** Server paths of the email auth routes. */
  paths: {
    login: string;
    signup: string;
    requestPasswordReset: string;
    resetPassword: string;
    verifyEmail: string;
  };
};

/**
 * Creates the client functions of the email auth flow.
 *
 * `ExtraSignupFields` are the app's user signup fields, which the signup form
 * sends along with the email and password.
 */
export function createEmailAuthActions<ExtraSignupFields extends object = {}>({
  api,
  paths,
}: EmailAuthActionsConfig): EmailAuthActions<ExtraSignupFields> {
  async function login(data: {
    email: string;
    password: string;
  }): Promise<void> {
    try {
      const { sessionId } = await api
        .post(paths.login, {
          json: data,
        })
        .json(SessionResponseSchema);
      await initSession(sessionId);
    } catch (e) {
      throw handleApiError(e);
    }
  }

  async function signup(
    data: EmailSignupData<ExtraSignupFields>,
  ): Promise<{ success: boolean }> {
    try {
      const { success } = await api
        .post(paths.signup, {
          json: data,
        })
        .json(SuccessResponseSchema);
      return { success };
    } catch (e) {
      throw handleApiError(e);
    }
  }

  async function requestPasswordReset(data: {
    email: string;
  }): Promise<{ success: boolean }> {
    try {
      const { success } = await api
        .post(paths.requestPasswordReset, {
          json: data,
        })
        .json(SuccessResponseSchema);
      return { success };
    } catch (e) {
      throw handleApiError(e);
    }
  }

  async function resetPassword(data: {
    token: string;
    password: string;
  }): Promise<{ success: boolean }> {
    try {
      const { success } = await api
        .post(paths.resetPassword, {
          json: data,
        })
        .json(SuccessResponseSchema);
      return { success };
    } catch (e) {
      throw handleApiError(e);
    }
  }

  async function verifyEmail(data: {
    token: string;
  }): Promise<{ success: boolean; reason?: string }> {
    try {
      const { success, reason } = await api
        .post(paths.verifyEmail, {
          json: data,
        })
        .json(SuccessResponseSchema);
      return { success, reason };
    } catch (e) {
      throw handleApiError(e);
    }
  }

  return { login, signup, requestPasswordReset, resetPassword, verifyEmail };
}
