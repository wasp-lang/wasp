{{={= =}=}}
import type { Prisma } from "@prisma/client";
import type * as AuthCore from "@wasp.sh/lib-auth";
import {
  createAuthUserHelpers,
  makeAuthUserIfPossible,
} from "@wasp.sh/lib-auth";
import type {
  {= userEntityName =},
  {= authEntityName =},
  {= authIdentityEntityName =},
} from "../entities/index.js";

// PRIVATE API
/**
 * The app's entity types and relation names that the auth lib's generics are
 * instantiated with.
 */
export type AppAuthEntityTypes = {
  user: {= userEntityName =};
  auth: {= authEntityName =};
  authIdentity: {= authIdentityEntityName =};
  userCreateInput: Prisma.{= userEntityName =}CreateInput;
  authWhereInput: Prisma.{= authEntityName =}WhereInput;
  authFieldOnUserEntity: "{= authFieldOnUserEntityName =}";
  userFieldOnAuthEntity: "{= userFieldOnAuthEntityName =}";
  identitiesFieldOnAuthEntity: "{= identitiesFieldOnAuthEntityName =}";
};

// PRIVATE API
export type EnabledProviderName =
  {=# enabledProviders =}
  | "{= providerName =}"
  {=/ enabledProviders =}
  | never;

const enabledProviders: EnabledProviderName[] = [
  {=# enabledProviders =}
  "{= providerName =}",
  {=/ enabledProviders =}
];

// PUBLIC API
export type AuthUser = AuthCore.AuthUser<
  AppAuthEntityTypes,
  EnabledProviderName
>;

// PRIVATE API (used in SDK and server)
export type AuthUserData = AuthCore.AuthUserData<
  AppAuthEntityTypes,
  EnabledProviderName
>;

// PRIVATE API
export type CompleteUserEntityWithAuth =
  AuthCore.CompleteUserEntityWithAuth<AppAuthEntityTypes>;

// PRIVATE API
export type CompleteAuthEntityWithIdentities =
  AuthCore.CompleteAuthEntityWithIdentities<AppAuthEntityTypes>;

// PRIVATE API
/**
 * User entity with all of the auth related data that's needed for the user facing
 * helper functions like `getUsername` and `getEmail`.
 */
export type UserEntityWithAuth = AuthCore.UserEntityWithAuth<AppAuthEntityTypes>;

// PRIVATE API (used in SDK and server)
export { makeAuthUserIfPossible };

const authUserHelpers = createAuthUserHelpers<
  AppAuthEntityTypes,
  EnabledProviderName
>({
  authFieldOnUserEntityName: "{= authFieldOnUserEntityName =}",
  identitiesFieldOnAuthEntityName: "{= identitiesFieldOnAuthEntityName =}",
  enabledProviders,
});

// PUBLIC API
export const { getEmail, getUsername, getFirstProviderUserId } = authUserHelpers;

// PRIVATE API
export const { createAuthUserData } = authUserHelpers;
