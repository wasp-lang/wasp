import type { Expand } from "@wasp.sh/lib-sdk-core";
import type { Request as ExpressRequest } from "express";
import type { AuthEntityTypes } from "../shared/entities.js";
import type { ProviderId } from "../shared/providerData.js";
import type { CreateUserResult } from "./identities.js";

/**
 * The app-specific types the auth hooks are typed with. Like `AuthEntityTypes`,
 * the generated code builds one of these and instantiates the hook types with it.
 */
export type AuthHookTypes = {
  /** `typeof prisma` of the app. */
  prisma: unknown;
  entities: AuthEntityTypes;
  /** The `OAuthData` union of the app's enabled OAuth providers. */
  oauthData: OAuthFlowData;
};

/** The part of `OAuthData` the framework needs. Providers extend it with their tokens. */
export type OAuthFlowData = {
  /**
   * Unique request ID that was generated during the OAuth flow.
   */
  uniqueRequestId: string;
};

export type OnBeforeSignupHook<H extends AuthHookTypes> = (
  params: Expand<OnBeforeSignupHookParams & InternalAuthHookParams<H>>,
) => void | Promise<void>;

export type OnAfterSignupHook<H extends AuthHookTypes> = (
  params: Expand<OnAfterSignupHookParams<H> & InternalAuthHookParams<H>>,
) => void | Promise<void>;

/**
 * Use this type for typing your `onAfterEmailVerified` hook.
 *
 * Wasp calls this hook exactly once, after a user successfully verifies their email during the email verification flow.
 *
 * @example
 * ```ts
 * export const onAfterEmailVerified: OnAfterEmailVerifiedHook = async ({
 *   email,
 * }) => {
 *   await emailSender.send({
 *     to: email,
 *     subject: 'Welcome to our platform!',
 *     text: `Your email ${email} has been verified successfully.`
 *   })
 * }
 * ```
 */
export type OnAfterEmailVerifiedHook<H extends AuthHookTypes> = (
  params: Expand<OnAfterEmailVerifiedHookParams<H> & InternalAuthHookParams<H>>,
) => void | Promise<void>;

/**
 * @returns Object with a URL that the OAuth flow should redirect to.
 */
export type OnBeforeOAuthRedirectHook<H extends AuthHookTypes> = (
  params: Expand<OnBeforeOAuthRedirectHookParams & InternalAuthHookParams<H>>,
) => { url: URL } | Promise<{ url: URL }>;

export type OnBeforeLoginHook<H extends AuthHookTypes> = (
  params: Expand<OnBeforeLoginHookParams<H> & InternalAuthHookParams<H>>,
) => void | Promise<void>;

export type OnAfterLoginHook<H extends AuthHookTypes> = (
  params: Expand<OnAfterLoginHookParams<H> & InternalAuthHookParams<H>>,
) => void | Promise<void>;

export type InternalAuthHookParams<H extends AuthHookTypes> = {
  /**
   * Prisma instance that can be used to interact with the database.
   */
  prisma: H["prisma"];
};

// NOTE: We should be exporting types that can be reached by users via other
// exported types (e.g. using the Parameters<T> Typescript helper).
// However, we are not exporting these types from the generated SDK to keep the
// API surface smaller. They are what the auth flows call the hooks with; the
// developer's hook gets them plus `InternalAuthHookParams`. Exporting them from
// the SDK might confuse users since the names are too similar to the exported
// function types.
export type OnBeforeSignupHookParams = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
   */
  providerId: ProviderId;
  /**
   * Request object that can be used to access the user's incoming signup request.
   */
  req: ExpressRequest;
};

export type OnAfterSignupHookParams<H extends AuthHookTypes> = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
   */
  providerId: ProviderId;
  /**
   * User object that was created during the signup process.
   */
  user: CreateUserResult<H["entities"]>;
  /**
   * OAuth flow data that was generated during the OAuth flow. This is only
   * available if the user signed up using OAuth.
   */
  oauth?: H["oauthData"];
  /**
   * Request object that can be used to access the user's incoming signup request.
   */
  req: ExpressRequest;
};

export type OnAfterEmailVerifiedHookParams<H extends AuthHookTypes> = {
  /**
   * The email address that was verified.
   */
  email: string;
  /**
   * The user who completed email verification.
   */
  user: LoggedInUser<H>;
  /**
   * Request object that can be used to access the user's incoming verification request.
   */
  req: ExpressRequest;
};

export type OnBeforeOAuthRedirectHookParams = {
  /**
   * URL that the OAuth flow should redirect to.
   */
  url: URL;
  /**
   * Unique request ID that was generated during the OAuth flow.
   */
  oauth: Pick<OAuthFlowData, "uniqueRequestId">;
  /**
   * Request object that can be used to access the user's incoming OAuth flow request.
   */
  req: ExpressRequest;
};

export type OnBeforeLoginHookParams<H extends AuthHookTypes> = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
   */
  providerId: ProviderId;
  /**
   * User that is trying to log in.
   */
  user: LoggedInUser<H>;
  /**
   * Request object that can be used to access the user's incoming login request.
   */
  req: ExpressRequest;
};

export type OnAfterLoginHookParams<H extends AuthHookTypes> = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
   */
  providerId: ProviderId;
  /**
   * User that is logged in.
   */
  user: LoggedInUser<H>;
  /**
   * OAuth flow data that was generated during the OAuth flow. This is only
   * available if the user logged in using OAuth.
   */
  oauth?: H["oauthData"];
  /**
   * Request object that can be used to access the user's incoming login request.
   */
  req: ExpressRequest;
};

type LoggedInUser<H extends AuthHookTypes> = H["entities"]["user"];

/** The hook functions the developer defined in the app. */
export type UserDefinedAuthHooks<H extends AuthHookTypes> = {
  onBeforeSignup?: OnBeforeSignupHook<H>;
  onAfterSignup?: OnAfterSignupHook<H>;
  onAfterEmailVerified?: OnAfterEmailVerifiedHook<H>;
  onBeforeOAuthRedirect?: OnBeforeOAuthRedirectHook<H>;
  onBeforeLogin?: OnBeforeLoginHook<H>;
  onAfterLogin?: OnAfterLoginHook<H>;
};

/**
 * The "internal hook functions" the auth flows call. They add the extra params
 * (e.g. `prisma`) before calling the developer's hook, and are no-ops for hooks
 * the developer didn't define.
 */
export type AuthHooks<H extends AuthHookTypes> = {
  onBeforeSignupHook: (
    params: OnBeforeSignupHookParams,
  ) => ReturnType<OnBeforeSignupHook<H>>;
  onAfterSignupHook: (
    params: OnAfterSignupHookParams<H>,
  ) => ReturnType<OnAfterSignupHook<H>>;
  onAfterEmailVerifiedHook: (
    params: OnAfterEmailVerifiedHookParams<H>,
  ) => ReturnType<OnAfterEmailVerifiedHook<H>>;
  onBeforeOAuthRedirectHook: (
    params: OnBeforeOAuthRedirectHookParams,
  ) => ReturnType<OnBeforeOAuthRedirectHook<H>>;
  onBeforeLoginHook: (
    params: OnBeforeLoginHookParams<H>,
  ) => ReturnType<OnBeforeLoginHook<H>>;
  onAfterLoginHook: (
    params: OnAfterLoginHookParams<H>,
  ) => ReturnType<OnAfterLoginHook<H>>;
};

export type AuthHooksConfig<H extends AuthHookTypes> = {
  prisma: H["prisma"];
  hooks: UserDefinedAuthHooks<H>;
};

export function createAuthHooks<H extends AuthHookTypes>({
  prisma,
  hooks,
}: AuthHooksConfig<H>): AuthHooks<H> {
  return {
    onBeforeSignupHook: hooks.onBeforeSignup
      ? withPrisma(hooks.onBeforeSignup)
      : async (_params) => {},
    onAfterSignupHook: hooks.onAfterSignup
      ? withPrisma(hooks.onAfterSignup)
      : async (_params) => {},
    onAfterEmailVerifiedHook: hooks.onAfterEmailVerified
      ? withPrisma(hooks.onAfterEmailVerified)
      : async (_params) => {},
    onBeforeOAuthRedirectHook: hooks.onBeforeOAuthRedirect
      ? withPrisma(hooks.onBeforeOAuthRedirect)
      : async (params) => params,
    onBeforeLoginHook: hooks.onBeforeLogin
      ? withPrisma(hooks.onBeforeLogin)
      : async (_params) => {},
    onAfterLoginHook: hooks.onAfterLogin
      ? withPrisma(hooks.onAfterLogin)
      : async (_params) => {},
  };

  /*
    We pass extra params to the user defined hook functions, but we don't want to
    pass the extra params (e.g. 'prisma') when we call the hooks in the auth flows.
  */
  function withPrisma<Params extends object, Result>(
    hook: (params: Expand<Params & InternalAuthHookParams<H>>) => Result,
  ): (params: Params) => Result {
    return (params) =>
      hook({
        prisma,
        ...params,
      } as unknown as Expand<Params & InternalAuthHookParams<H>>);
  }
}
