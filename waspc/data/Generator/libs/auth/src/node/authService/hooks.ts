import type { ProviderId } from "../providerData";
import type { MaybePromise } from "./types";

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
