import { isNotNull, type Expand } from "@wasp.sh/lib-sdk-core";
import type { AuthEntityTypes } from "./entities.js";
import {
  getProviderData,
  type PossibleProviderData,
  type ProviderName,
} from "./providerData.js";

/**
 * The user entity with the auth data that the server loads for a session,
 * from which `AuthUser` is built.
 */
export type CompleteUserEntityWithAuth<T extends AuthEntityTypes> =
  MakeUserEntityWithAuth<T, CompleteAuthEntityWithIdentities<T>>;

export type CompleteAuthEntityWithIdentities<T extends AuthEntityTypes> =
  MakeAuthEntityWithIdentities<T, T["authIdentity"]>;

/**
 * User entity with all of the auth related data that's needed for the user facing
 * helper functions like `getUsername` and `getEmail`.
 */
export type UserEntityWithAuth<T extends AuthEntityTypes> =
  MakeUserEntityWithAuth<
    T,
    MakeAuthEntityWithIdentities<
      T,
      // It's constructed like the Complete* types, but only with the fields needed
      // for the user facing functions.
      Pick<T["authIdentity"], "providerName" | "providerUserId">
    >
  >;

type MakeUserEntityWithAuth<T extends AuthEntityTypes, AuthType> = T["user"] & {
  [K in T["authFieldOnUserEntity"]]: AuthType | null;
};

type MakeAuthEntityWithIdentities<
  T extends AuthEntityTypes,
  IdentityType,
> = T["auth"] & {
  [K in T["identitiesFieldOnAuthEntity"]]: IdentityType[];
};

export type UserFacingProviderData<PN extends ProviderName> = {
  id: string;
} & Omit<PossibleProviderData[PN], "hashedPassword">;

export type AuthUserIdentities<EnabledProvider extends ProviderName> = {
  [PN in EnabledProvider]: Expand<UserFacingProviderData<PN>> | null;
};

/*
 * We write this type out instead of using `ReturnType<typeof createAuthUserData>`
 * because `Expand` makes editors show each identity's fields on hover. The
 * inferred type would show `UserFacingProviderData<"email">` instead.
 */
export type AuthUserData<
  T extends AuthEntityTypes,
  EnabledProvider extends ProviderName,
> = Omit<CompleteUserEntityWithAuth<T>, T["authFieldOnUserEntity"]> & {
  identities: AuthUserIdentities<EnabledProvider>;
};

export type AuthUserMethods = {
  getFirstProviderUserId: () => string | null;
};

export type AuthUser<
  T extends AuthEntityTypes,
  EnabledProvider extends ProviderName,
> = AuthUserData<T, EnabledProvider> & AuthUserMethods;

export type AuthUserConfig<
  T extends AuthEntityTypes,
  EnabledProvider extends ProviderName,
> = {
  authFieldOnUserEntityName: T["authFieldOnUserEntity"];
  identitiesFieldOnAuthEntityName: T["identitiesFieldOnAuthEntity"];
  /**
   * The providers enabled in the app, in the order their identities appear on
   * `AuthUser.identities` (which is also the order `getFirstProviderUserId` uses).
   */
  enabledProviders: readonly EnabledProvider[];
};

export type AuthUserHelpers<
  T extends AuthEntityTypes,
  EnabledProvider extends ProviderName,
> = {
  createAuthUserData: (
    user: CompleteUserEntityWithAuth<T>,
  ) => AuthUserData<T, EnabledProvider>;
  getEmail: (user: UserEntityWithAuth<T>) => string | null;
  getUsername: (user: UserEntityWithAuth<T>) => string | null;
  getFirstProviderUserId: (user?: UserEntityWithAuth<T>) => string | null;
};

/**
 * Creates the functions that shape the app's user entity into `AuthUser` data
 * and read identities off it.
 */
export function createAuthUserHelpers<
  T extends AuthEntityTypes,
  EnabledProvider extends ProviderName,
>(
  config: AuthUserConfig<T, EnabledProvider>,
): AuthUserHelpers<T, EnabledProvider> {
  const { authFieldOnUserEntityName, identitiesFieldOnAuthEntityName } = config;

  function createAuthUserData(
    user: CompleteUserEntityWithAuth<T>,
  ): AuthUserData<T, EnabledProvider> {
    const { [authFieldOnUserEntityName]: auth, ...rest } = user as Record<
      string,
      unknown
    >;
    if (!auth) {
      throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`);
    }
    const identities = {} as AuthUserIdentities<EnabledProvider>;
    for (const providerName of config.enabledProviders) {
      identities[providerName] = getProviderInfo(
        auth as CompleteAuthEntityWithIdentities<T>,
        providerName,
      );
    }
    return {
      ...rest,
      identities,
    } as AuthUserData<T, EnabledProvider>;
  }

  function getEmail(user: UserEntityWithAuth<T>): string | null {
    return findUserIdentity(user, "email")?.providerUserId ?? null;
  }

  function getUsername(user: UserEntityWithAuth<T>): string | null {
    return findUserIdentity(user, "username")?.providerUserId ?? null;
  }

  function getFirstProviderUserId(user?: UserEntityWithAuth<T>): string | null {
    const identities = user ? getIdentities(user) : null;
    if (!identities || identities.length === 0) {
      return null;
    }

    return identities[0]?.providerUserId ?? null;
  }

  function getProviderInfo<PN extends ProviderName>(
    auth: CompleteAuthEntityWithIdentities<T>,
    providerName: PN,
  ): Expand<UserFacingProviderData<PN>> | null {
    const identity = getIdentity(auth, providerName);
    if (!identity) {
      return null;
    }
    const providerInfo: UserFacingProviderData<PN> = {
      ...getProviderData<PN>(identity.providerData),
      id: identity.providerUserId,
    };
    // `Expand` only changes how the type is displayed.
    return providerInfo as unknown as Expand<UserFacingProviderData<PN>>;
  }

  function getIdentity(
    auth: CompleteAuthEntityWithIdentities<T>,
    providerName: ProviderName,
  ): T["authIdentity"] | null {
    const identities = (auth as Record<string, unknown>)[
      identitiesFieldOnAuthEntityName
    ] as T["authIdentity"][];
    return identities.find((i) => i.providerName === providerName) ?? null;
  }

  function findUserIdentity(
    user: UserEntityWithAuth<T>,
    providerName: ProviderName,
  ): UserIdentity | null {
    const identities = getIdentities(user);
    if (!identities) {
      return null;
    }
    return (
      identities.find((identity) => identity.providerName === providerName) ??
      null
    );
  }

  // The relation names are only known at runtime, so the lookups go through
  // string keys and the results are typed by hand.
  function getIdentities(user: UserEntityWithAuth<T>): UserIdentity[] | null {
    const auth = (user as Record<string, unknown>)[
      authFieldOnUserEntityName
    ] as Record<string, unknown> | null;
    if (!auth) {
      return null;
    }
    return auth[identitiesFieldOnAuthEntityName] as UserIdentity[];
  }

  return {
    createAuthUserData,
    getEmail,
    getUsername,
    getFirstProviderUserId,
  };
}

type UserIdentity = Pick<
  AuthEntityTypes["authIdentity"],
  "providerName" | "providerUserId"
>;

type AuthUserDataShape = {
  identities: { [providerName: string]: { id: string } | null };
};

export function makeAuthUserIfPossible(user: null): null;
export function makeAuthUserIfPossible<Data extends AuthUserDataShape>(
  user: Data,
): Data & AuthUserMethods;
export function makeAuthUserIfPossible<Data extends AuthUserDataShape>(
  user: Data | null,
): (Data & AuthUserMethods) | null;
export function makeAuthUserIfPossible<Data extends AuthUserDataShape>(
  user: Data | null,
): (Data & AuthUserMethods) | null {
  return user ? makeAuthUser(user) : null;
}

function makeAuthUser<Data extends AuthUserDataShape>(
  data: Data,
): Data & AuthUserMethods {
  return {
    ...data,
    getFirstProviderUserId: () => {
      const identities = Object.values(data.identities).filter(isNotNull);
      return identities[0]?.id ?? null;
    },
  };
}
