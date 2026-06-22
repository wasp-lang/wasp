import type { ProviderId, ProviderName } from "../providerData";

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
