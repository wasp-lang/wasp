import type { AuthHooks } from "../authService/hooks";
import type { SessionService } from "../authService/session";
import type {
  AuthId,
  AuthIdentity,
  AuthWithUser,
  CreatedUserWithAuth,
  MaybePromise,
  ProviderIdFor,
} from "../authService/types";

type UsernameSignupRepository<
  CreatedUser,
  UserFields extends object = object,
> = {
  createUserWithIdentity(args: {
    providerId: ProviderIdFor<"username">;
    serializedProviderData?: string;
    userFields?: UserFields;
  }): Promise<CreatedUserWithAuth<CreatedUser>>;
};

type UsernameLoginRepository<User> = {
  findIdentity(
    providerId: ProviderIdFor<"username">,
  ): Promise<AuthIdentity<"username"> | null>;
  findAuthWithUserByAuthId(authId: AuthId): Promise<AuthWithUser<User> | null>;
};

export type UsernameSignupAdapters<
  RequestContext,
  CreatedUser,
  UserFields extends object = object,
> = {
  authRepository: UsernameSignupRepository<CreatedUser, UserFields>;
  hooks: Pick<
    AuthHooks<RequestContext, never, CreatedUser>,
    "onBeforeSignup" | "onAfterSignup"
  >;
};

export type UsernameLoginAdapters<RequestContext, User> = {
  authRepository: UsernameLoginRepository<User>;
  sessionService: SessionService;
  hooks: Pick<
    AuthHooks<RequestContext, User>,
    "onBeforeLogin" | "onAfterLogin"
  >;
};

export type UsernameSignupArgs<
  RequestContext,
  CreatedUser,
  UserFields extends object = object,
> = {
  fields: object;
  request: RequestContext;
  getUserFields?: (fields: object) => MaybePromise<UserFields>;
  adapters: UsernameSignupAdapters<RequestContext, CreatedUser, UserFields>;
};

export type UsernameLoginArgs<RequestContext, User> = {
  fields: object;
  request: RequestContext;
  adapters: UsernameLoginAdapters<RequestContext, User>;
};

export type UsernameSignupResult = {
  success: true;
};

export type UsernameAuthLoginResult = {
  sessionId: string;
};
