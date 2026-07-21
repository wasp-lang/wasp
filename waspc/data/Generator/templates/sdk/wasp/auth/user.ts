{{={= =}=}}
import type {
  {= userEntityName =},
  {= authEntityName =},
  {= authIdentityEntityName =},
} from '../entities/index.js'
import type { PossibleProviderData, ProviderName } from './utils.js'
import type { Expand } from '../universal/types.js'
import { isNotNull } from '../universal/predicates.js'

/**
 * We split the user code into two files:
 * - This one, with client-safe code (types and helpers).
 * - `wasp/server/auth/user.ts`, with server-only code.
 */

/**
 * {@link AuthUser} must be declared in a module which is directly reachable
 * through the package's `exports` map (this module is reachable as
 * `"wasp/auth/user"`).
 *
 * User code is compiled with declaration emit enabled (`declaration: true`).
 * This is because the server and the client TypeScript project reference
 * the user's TypeScript project. As such, the user project must have
 * `composite: true` flag which makes `declaration` default to `true`.
 *
 * When declaration emit is enabled, every exported binding without an
 * explicit type annotation gets its inferred type serialized into a
 * `.d.ts` file.
 * 
 * A common example is users defining operations while typing them with
 * the `satisfies` keyword:
 * ```ts
 * import type { GetNumberOfTasks } from "wasp/server/operations";
 * 
 * export const GetNumberOfTasks = (async (_args, context) => {
 *   return context.entities.Task.count();
 * }) satisfies GetNumberOfTasks<void>;
 * ```
 *
 * If that inferred type structurally contains a type symbol from a
 * dependency package, `tsc` must synthesize a portable reference to
 * that symbol.
 * E.g., an authenticated operation's context contains a type symbol for
 * {@link AuthUser}. So we must be able to create a portable reference to
 * {@link AuthUser}.
 *
 * Because {@link AuthUser} is declared here, in an exports-reachable module,
 * `tsc` can always create a portable reference to it, no matter what the
 * user's code imports. If it were declared in a non-exported module (e.g.
 * `wasp/server/auth/user`), `tsc` could name it only when the user's program
 * happened to load some module re-exporting it, which is not guaranteed.
 */

// PUBLIC API
export type AuthUser = AuthUserData & {
  getFirstProviderUserId: () => string | null,
}

// PRIVATE API
/*
 * Ideally, we'd do something like this:
 * ```
 * export type AuthUserData = ReturnType<typeof createAuthUserData>
 * ```
 * to get the benefits of the createAuthUser and the AuthUserData type being in sync.
 *
 * But since we are not using strict mode, the inferred return type of createAuthUser
 * is not correct. So we have to define the AuthUserData type manually.
 *
 * TODO: Change this once/if we switch to strict mode. https://github.com/wasp-lang/wasp/issues/1938
 */
export type AuthUserData = Omit<CompleteUserEntityWithAuth, '{= authFieldOnUserEntityName =}'> & {
  identities: {
    {=# enabledProviders.isEmailAuthEnabled =}
    email: Expand<UserFacingProviderData<'email'>> | null
    {=/ enabledProviders.isEmailAuthEnabled =}
    {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
    username: Expand<UserFacingProviderData<'username'>> | null
    {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
    {=# enabledProviders.isSlackAuthEnabled =}
    slack: Expand<UserFacingProviderData<'slack'>> | null
    {=/ enabledProviders.isSlackAuthEnabled =}
    {=# enabledProviders.isDiscordAuthEnabled =}
    discord: Expand<UserFacingProviderData<'discord'>> | null
    {=/ enabledProviders.isDiscordAuthEnabled =}
    {=# enabledProviders.isGoogleAuthEnabled =}
    google: Expand<UserFacingProviderData<'google'>> | null
    {=/ enabledProviders.isGoogleAuthEnabled =}
    {=# enabledProviders.isKeycloakAuthEnabled =}
    keycloak: Expand<UserFacingProviderData<'keycloak'>> | null
    {=/ enabledProviders.isKeycloakAuthEnabled =}
    {=# enabledProviders.isGitHubAuthEnabled =}
    github: Expand<UserFacingProviderData<'github'>> | null
    {=/ enabledProviders.isGitHubAuthEnabled =}
    {=# enabledProviders.isMicrosoftAuthEnabled =}
    microsoft: Expand<UserFacingProviderData<'microsoft'>> | null
    {=/ enabledProviders.isMicrosoftAuthEnabled =}
  },
}

// PRIVATE API (used in SDK and server)
export type UserFacingProviderData<PN extends ProviderName> = {
  id: string
} & Omit<PossibleProviderData[PN], 'hashedPassword'>

// PRIVATE API
export type CompleteUserEntityWithAuth =
  MakeUserEntityWithAuth<CompleteAuthEntityWithIdentities>

// PRIVATE API
export type CompleteAuthEntityWithIdentities =
  MakeAuthEntityWithIdentities<{= authIdentityEntityName =}>

// PRIVATE API
/**
 * User entity with all of the auth related data that's needed for the user facing
 * helper functions like `getUsername` and `getEmail`.
 */
export type UserEntityWithAuth = MakeUserEntityWithAuth<
  MakeAuthEntityWithIdentities<
    // It's constructed like the Complete* types, but only with the fields needed
    // for the user facing functions.
    Pick<{= authIdentityEntityName =}, 'providerName' | 'providerUserId'>
  >
>

type MakeUserEntityWithAuth<AuthType> = {= userEntityName =} & {
  {= authFieldOnUserEntityName =}: AuthType | null
}

type MakeAuthEntityWithIdentities<IdentityType> = {= authEntityName =} & {
  {= identitiesFieldOnAuthEntityName =}: IdentityType[]
}

// PUBLIC API
export function getEmail(user: UserEntityWithAuth): string | null {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

// PUBLIC API
export function getUsername(user: UserEntityWithAuth): string | null {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

// PUBLIC API
export function getFirstProviderUserId(user?: UserEntityWithAuth): string | null {
  if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
    return null;
  }

  return user.auth.identities[0].providerUserId ?? null;
}

// PRIVATE API (used in SDK and server)
export function makeAuthUserIfPossible(user: null): null
export function makeAuthUserIfPossible(user: AuthUserData): AuthUser
export function makeAuthUserIfPossible(user: AuthUserData | null): AuthUser | null
export function makeAuthUserIfPossible(
  user: AuthUserData | null,
): AuthUser | null {
  return user ? makeAuthUser(user) : null
}

function makeAuthUser(data: AuthUserData): AuthUser {
  return {
    ...data,
    getFirstProviderUserId: () => {
      const identities = Object.values(data.identities).filter(isNotNull);
      return identities.length > 0 ? identities[0].id : null;
    },
  };
}

function findUserIdentity(user: UserEntityWithAuth, providerName: ProviderName): NonNullable<UserEntityWithAuth['auth']>['identities'][number] | null {
  if (!user.auth) {
    return null;
  }
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  ) ?? null;
}
