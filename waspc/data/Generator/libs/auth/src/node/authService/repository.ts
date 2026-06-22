import type { PossibleProviderData, ProviderName } from "../providerData";
import type {
  AuthId,
  AuthIdentity,
  AuthWithUser,
  CreatedUserWithAuth,
  ProviderIdFor,
} from "./types";

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
