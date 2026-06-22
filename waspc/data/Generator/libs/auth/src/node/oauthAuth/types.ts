import type { AuthHooks } from "../authService/hooks";
import type { OAuthRedirects } from "../authService/oauth";
import type { OneTimeCodeStore } from "../authService/oneTimeCode";
import type { SessionService } from "../authService/session";
import type {
  AuthId,
  AuthIdentity,
  AuthWithUser,
  CreatedUserWithAuth,
  MaybePromise,
  ProviderIdFor,
} from "../authService/types";
import type { ProviderName } from "../providerData";

export type OAuthProviderName = Exclude<ProviderName, "email" | "username">;

export type OAuthHookData = {
  uniqueRequestId: string;
  providerName: OAuthProviderName;
  tokens: unknown;
};

type OAuthCallbackRepository<CreatedUser, User, UserFields extends object> = {
  findIdentity(
    providerId: ProviderIdFor<OAuthProviderName>,
  ): Promise<AuthIdentity<OAuthProviderName> | null>;
  findAuthWithUserByAuthId(authId: AuthId): Promise<AuthWithUser<User> | null>;
  createUserWithIdentity(args: {
    providerId: ProviderIdFor<OAuthProviderName>;
    serializedProviderData?: string;
    userFields?: UserFields;
  }): Promise<CreatedUserWithAuth<CreatedUser>>;
};

type OAuthCodeExchangeRepository<User> = {
  findAuthWithUserByAuthId(authId: AuthId): Promise<AuthWithUser<User> | null>;
};

export type OAuthCallbackAdapters<
  RequestContext,
  User,
  CreatedUser,
  UserFields extends object,
  OAuthData extends OAuthHookData,
> = {
  authRepository: OAuthCallbackRepository<CreatedUser, User, UserFields>;
  hooks: Pick<
    AuthHooks<RequestContext, User, CreatedUser, OAuthData>,
    "onBeforeLogin" | "onAfterLogin" | "onBeforeSignup" | "onAfterSignup"
  >;
  oneTimeCodeStore: Pick<OneTimeCodeStore, "createToken">;
  oauthRedirects: Pick<OAuthRedirects, "getRedirectUrlForOneTimeCode">;
};

export type OAuthCallbackArgs<
  RequestContext,
  User,
  CreatedUser,
  UserFields extends object,
  OAuthData extends OAuthHookData,
> = {
  providerName: OAuthProviderName;
  providerUserId: string;
  providerProfile: unknown;
  request: RequestContext;
  oauth: OAuthData;
  getUserFields?: (providerProfile: unknown) => MaybePromise<UserFields>;
  adapters: OAuthCallbackAdapters<
    RequestContext,
    User,
    CreatedUser,
    UserFields,
    OAuthData
  >;
};

export type OAuthCallbackResult = {
  redirectUrl: URL;
};

export type OAuthCallbackErrorRedirectArgs = {
  error: unknown;
  adapters: {
    oauthRedirects: Pick<OAuthRedirects, "getFailureRedirectUrl">;
  };
};

export type OAuthCodeExchangeAdapters<User> = {
  oneTimeCodeStore: Pick<
    OneTimeCodeStore,
    "verifyToken" | "isUsed" | "markUsed"
  >;
  authRepository: OAuthCodeExchangeRepository<User>;
  sessionService: SessionService;
};

export type OAuthCodeExchangeArgs<User> = {
  fields: object;
  adapters: OAuthCodeExchangeAdapters<User>;
};

export type OAuthCodeExchangeResult = {
  sessionId: string;
};
