import type {
  PossibleProviderData,
  ProviderId,
  ProviderName,
} from "./providerData";

export type MaybePromise<T> = T | Promise<T>;

export type AuthId = string;

export type ProviderIdFor<PN extends ProviderName> = ProviderId & {
  providerName: PN;
};

export type AuthIdentity<PN extends ProviderName = ProviderName> = {
  authId: AuthId;
  providerName: PN;
  providerUserId: string;
  providerData: string;
};

export type AuthWithUser<User> = {
  authId: AuthId;
  user: User;
};

export type CreatedUserWithAuth<CreatedUser> = {
  authId: AuthId;
  user: CreatedUser;
};

export type Session = {
  id: string;
};

export type AuthRepository<
  User,
  CreatedUser = User,
  UserFields extends object = object,
> = {
  findIdentity<PN extends ProviderName>(
    providerId: ProviderIdFor<PN>,
  ): Promise<AuthIdentity<PN> | null>;
  findAuthWithUserByAuthId(authId: AuthId): Promise<AuthWithUser<User> | null>;
  createUserWithIdentity<PN extends ProviderName>(args: {
    providerId: ProviderIdFor<PN>;
    serializedProviderData?: string;
    userFields?: UserFields;
  }): Promise<CreatedUserWithAuth<CreatedUser>>;
  deleteUserByAuthId(authId: AuthId): Promise<{ count: number }>;
  updateIdentityProviderData<PN extends ProviderName>(args: {
    providerId: ProviderIdFor<PN>;
    existingProviderData: PossibleProviderData[PN];
    providerDataUpdates: Partial<PossibleProviderData[PN]>;
  }): Promise<AuthIdentity<PN>>;
};

export type SessionService = {
  createSession(authId: AuthId): Promise<Session>;
};

export type EmailVerificationService = {
  createVerificationLink(email: string): Promise<string>;
  sendVerificationEmail(args: {
    email: string;
    verificationLink: string;
  }): Promise<void>;
};

export type PasswordResetService = {
  createPasswordResetLink(email: string): Promise<string>;
  sendPasswordResetEmail(args: {
    email: string;
    passwordResetLink: string;
  }): Promise<void>;
};

export type EmailTokenService = {
  verifyEmailToken(token: string): Promise<{ email: string }>;
};

export type OneTimeCodeStore = {
  createToken(authId: AuthId): Promise<string>;
  verifyToken(code: string): Promise<{ authId: AuthId }>;
  isUsed(code: string): boolean;
  markUsed(code: string): void;
};

export type OAuthRedirects = {
  getRedirectUrlForOneTimeCode(code: string): URL;
  getFailureRedirectUrl(error: unknown): URL;
};

export type AuthHooks<
  RequestContext,
  User,
  CreatedUser = User,
  OAuthData = never,
> = {
  onBeforeSignup(args: {
    request: RequestContext;
    providerId: ProviderId;
  }): MaybePromise<void>;
  onAfterSignup(args: {
    request: RequestContext;
    providerId: ProviderId;
    user: CreatedUser;
    oauth?: OAuthData;
  }): MaybePromise<void>;
  onAfterEmailVerified(args: {
    request: RequestContext;
    email: string;
    user: User;
  }): MaybePromise<void>;
  onBeforeLogin(args: {
    request: RequestContext;
    providerId: ProviderId;
    user: User;
  }): MaybePromise<void>;
  onAfterLogin(args: {
    request: RequestContext;
    providerId: ProviderId;
    user: User;
    oauth?: OAuthData;
  }): MaybePromise<void>;
  onBeforeOAuthRedirect(args: {
    request: RequestContext;
    url: URL;
    oauth: { uniqueRequestId: string };
  }): MaybePromise<{ url: URL }>;
};

export type Clock = {
  now(): Date;
};

export type RandomSource = {
  integer(args: { min: number; max: number }): number;
};

export type WorkSimulator = {
  doFakeWork(): Promise<void>;
};

export type AuthCoreAdapters<
  RequestContext,
  User,
  CreatedUser = User,
  UserFields extends object = object,
  OAuthData = never,
> = {
  authRepository: AuthRepository<User, CreatedUser, UserFields>;
  sessionService: SessionService;
  hooks: AuthHooks<RequestContext, User, CreatedUser, OAuthData>;
  clock: Clock;
  random: RandomSource;
  workSimulator: WorkSimulator;
};

export type EmailAuthAdapters<
  RequestContext,
  User,
  CreatedUser = User,
  UserFields extends object = object,
> = AuthCoreAdapters<RequestContext, User, CreatedUser, UserFields> & {
  emailVerification: EmailVerificationService;
  passwordReset: PasswordResetService;
  emailTokens: EmailTokenService;
};

export type OAuthAuthAdapters<
  RequestContext,
  User,
  CreatedUser = User,
  UserFields extends object = object,
  OAuthData = never,
> = AuthCoreAdapters<
  RequestContext,
  User,
  CreatedUser,
  UserFields,
  OAuthData
> & {
  oneTimeCodeStore: OneTimeCodeStore;
  oauthRedirects: OAuthRedirects;
};

export type AuthServiceAdapters<
  RequestContext,
  User,
  CreatedUser = User,
  UserFields extends object = object,
  OAuthData = never,
> = AuthCoreAdapters<
  RequestContext,
  User,
  CreatedUser,
  UserFields,
  OAuthData
> & {
  emailVerification: EmailVerificationService;
  passwordReset: PasswordResetService;
  emailTokens: EmailTokenService;
  oneTimeCodeStore: OneTimeCodeStore;
  oauthRedirects: OAuthRedirects;
};

export type AuthServiceErrorCode =
  | "invalid-credentials"
  | "invalid-token"
  | "used-token"
  | "missing-token"
  | "email-resend-too-soon"
  | "email-not-verified"
  | "email-delivery-failed"
  | "identity-already-exists"
  | "validation-failed"
  | "oauth-flow-failed"
  | "save-failed";

export class AuthServiceError extends Error {
  constructor(
    public readonly code: AuthServiceErrorCode,
    message: string,
    public readonly metadata: Record<string, unknown> = {},
  ) {
    super(message);
    this.name = "AuthServiceError";
  }
}
