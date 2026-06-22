import type { EmailVerificationService } from "../authService/email";
import type { AuthHooks } from "../authService/hooks";
import type { Clock, WorkSimulator } from "../authService/runtime";
import type { SessionService } from "../authService/session";
import type {
  AuthId,
  AuthIdentity,
  AuthWithUser,
  CreatedUserWithAuth,
  MaybePromise,
  ProviderIdFor,
} from "../authService/types";

type EmailSignupRepository<CreatedUser, UserFields extends object = object> = {
  findIdentity(
    providerId: ProviderIdFor<"email">,
  ): Promise<AuthIdentity<"email"> | null>;
  createUserWithIdentity(args: {
    providerId: ProviderIdFor<"email">;
    serializedProviderData?: string;
    userFields?: UserFields;
  }): Promise<CreatedUserWithAuth<CreatedUser>>;
  deleteUserByAuthId(authId: AuthId): Promise<{ count: number }>;
};

type EmailLoginRepository<User> = {
  findIdentity(
    providerId: ProviderIdFor<"email">,
  ): Promise<AuthIdentity<"email"> | null>;
  findAuthWithUserByAuthId(authId: AuthId): Promise<AuthWithUser<User> | null>;
};

export type EmailSignupAdapters<
  RequestContext,
  CreatedUser,
  UserFields extends object = object,
> = {
  authRepository: EmailSignupRepository<CreatedUser, UserFields>;
  hooks: Pick<
    AuthHooks<RequestContext, never, CreatedUser>,
    "onBeforeSignup" | "onAfterSignup"
  >;
  emailVerification: EmailVerificationService;
  clock: Clock;
  workSimulator: WorkSimulator;
};

export type EmailLoginAdapters<RequestContext, User> = {
  authRepository: EmailLoginRepository<User>;
  sessionService: SessionService;
  hooks: Pick<
    AuthHooks<RequestContext, User>,
    "onBeforeLogin" | "onAfterLogin"
  >;
};

export type EmailSignupArgs<
  RequestContext,
  CreatedUser,
  UserFields extends object = object,
> = {
  fields: object;
  request: RequestContext;
  getUserFields?: (fields: object) => MaybePromise<UserFields>;
  isEmailAutoVerified: boolean;
  adapters: EmailSignupAdapters<RequestContext, CreatedUser, UserFields>;
};

export type EmailLoginArgs<RequestContext, User> = {
  fields: object;
  request: RequestContext;
  adapters: EmailLoginAdapters<RequestContext, User>;
};

export type EmailSignupResult = {
  success: true;
};

export type EmailAuthLoginResult = {
  sessionId: string;
};
