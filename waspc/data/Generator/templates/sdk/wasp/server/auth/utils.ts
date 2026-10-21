{{={= =}=}}
import type * as AuthCore from "@wasp.sh/lib-auth/node";
import { createAuthIdentities } from "@wasp.sh/lib-auth/node";
import type { AppAuthEntityTypes } from "../../auth/user.js";
import { prisma } from "../index.js";

// Runtime-agnostic provider data code, re-exported here because it's part of
// the server-side auth API surface (e.g. through `wasp/server/auth`).
export {
  createInvalidCredentialsError,
  createProviderId,
  normalizeProviderUserId,
  getProviderData,
  getProviderDataWithPassword,
  type ProviderId,
  type ProviderName,
  type PossibleProviderData,
  type EmailProviderData,
  type UsernameProviderData,
  type OAuthProviderData,
} from "@wasp.sh/lib-auth";
export {
  doFakeWork,
  sanitizeAndSerializeProviderData,
} from "@wasp.sh/lib-auth/node";

// PRIVATE API
export const contextWithUserEntity = {
  entities: {
    {= userEntityUpper =}: prisma.{= userEntityLower =},
  },
};

// PRIVATE API
export const authConfig = {
  failureRedirectPath: {=& failureRedirectPath =},
  successRedirectPath: {=& successRedirectPath =},
};

// PRIVATE API
export const authIdentities = createAuthIdentities<AppAuthEntityTypes>({
  prisma: {
    user: prisma.{= userEntityLower =},
    auth: prisma.{= authEntityLower =},
    authIdentity: prisma.{= authIdentityEntityLower =},
  },
  userEntityName: "{= userEntityUpper =}",
  authFieldOnUserEntityName: "{= authFieldOnUserEntityName =}",
  userFieldOnAuthEntityName: "{= userFieldOnAuthEntityName =}",
  identitiesFieldOnAuthEntityName: "{= identitiesFieldOnAuthEntityName =}",
});

// PUBLIC API
export const { findAuthIdentity, updateAuthIdentityProviderData, createUser } =
  authIdentities;

// PRIVATE API
export const {
  findAuthWithUserBy,
  deleteUserByAuthId,
  rethrowPossibleAuthError,
  validateAndGetUserFields,
} = authIdentities;

// PRIVATE API
export type FindAuthWithUserResult =
  AuthCore.FindAuthWithUserResult<AppAuthEntityTypes>;

// PUBLIC API
export type CreateUserResult = AuthCore.CreateUserResult<AppAuthEntityTypes>;
