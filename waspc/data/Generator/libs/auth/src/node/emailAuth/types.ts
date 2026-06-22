import type {
  EmailTokenService,
  EmailVerificationService,
  PasswordResetService,
} from "../authService/email";
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
import type { EmailProviderData } from "../providerData";

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

type EmailPasswordResetRequestRepository = {
  findIdentity(
    providerId: ProviderIdFor<"email">,
  ): Promise<AuthIdentity<"email"> | null>;
};

type EmailPasswordResetRepository = {
  findIdentity(
    providerId: ProviderIdFor<"email">,
  ): Promise<AuthIdentity<"email"> | null>;
  updateIdentityProviderData(args: {
    providerId: ProviderIdFor<"email">;
    existingProviderData: EmailProviderData;
    providerDataUpdates: Partial<EmailProviderData>;
  }): Promise<AuthIdentity<"email">>;
};

type EmailVerificationRepository<User> = EmailPasswordResetRepository & {
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

export type EmailPasswordResetRequestAdapters = {
  authRepository: EmailPasswordResetRequestRepository;
  passwordReset: PasswordResetService;
  clock: Clock;
  workSimulator: WorkSimulator;
};

export type EmailPasswordResetAdapters = {
  authRepository: EmailPasswordResetRepository;
  tokenService: EmailTokenService;
};

export type EmailVerificationAdapters<RequestContext, User> = {
  authRepository: EmailVerificationRepository<User>;
  tokenService: EmailTokenService;
  hooks: Pick<AuthHooks<RequestContext, User>, "onAfterEmailVerified">;
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

export type EmailPasswordResetRequestArgs = {
  fields: object;
  adapters: EmailPasswordResetRequestAdapters;
};

export type EmailPasswordResetArgs = {
  fields: object;
  adapters: EmailPasswordResetAdapters;
};

export type EmailVerificationArgs<RequestContext, User> = {
  fields: object;
  request: RequestContext;
  adapters: EmailVerificationAdapters<RequestContext, User>;
};

export type EmailSignupResult = {
  success: true;
};

export type EmailAuthLoginResult = {
  sessionId: string;
};

export type EmailPasswordResetRequestResult = {
  success: true;
};

export type EmailPasswordResetResult = {
  success: true;
};

export type EmailVerificationResult = {
  success: true;
};
