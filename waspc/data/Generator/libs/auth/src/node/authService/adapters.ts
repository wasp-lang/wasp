import type {
  EmailTokenService,
  EmailVerificationService,
  PasswordResetService,
} from "./email";
import type { AuthHooks } from "./hooks";
import type { OneTimeCodeStore } from "./oneTimeCode";
import type { OAuthRedirects } from "./oauth";
import type { AuthRepository } from "./repository";
import type { Clock, RandomSource, WorkSimulator } from "./runtime";
import type { SessionService } from "./session";

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
