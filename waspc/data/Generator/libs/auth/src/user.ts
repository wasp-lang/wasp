type UserIdentity = {
  providerName: string;
  providerUserId: string;
};

type UserWithAuth = {
  auth: {
    identities?: UserIdentity[] | null;
  } | null;
};

type AuthUserIdentity = {
  id: string;
} | null;

type AuthUserDataWithIdentities = {
  identities: Record<string, AuthUserIdentity | undefined>;
};

export type AuthUserWithHelpers<AuthUserData> = AuthUserData & {
  getFirstProviderUserId: () => string | null;
};

export function getEmail(user: UserWithAuth): string | null {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

export function getUsername(user: UserWithAuth): string | null {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

export function getFirstProviderUserId(user?: UserWithAuth): string | null {
  const identities = user?.auth?.identities;
  if (!identities || identities.length === 0) {
    return null;
  }

  return identities[0]?.providerUserId ?? null;
}

export function makeAuthUserIfPossible(user: null): null;
export function makeAuthUserIfPossible<AuthUserData extends AuthUserDataWithIdentities>(
  user: AuthUserData,
): AuthUserWithHelpers<AuthUserData>;
export function makeAuthUserIfPossible<AuthUserData extends AuthUserDataWithIdentities>(
  user: AuthUserData | null,
): AuthUserWithHelpers<AuthUserData> | null;
export function makeAuthUserIfPossible<AuthUserData extends AuthUserDataWithIdentities>(
  user: AuthUserData | null,
): AuthUserWithHelpers<AuthUserData> | null {
  return user ? makeAuthUser(user) : null;
}

function makeAuthUser<AuthUserData extends AuthUserDataWithIdentities>(
  data: AuthUserData,
): AuthUserWithHelpers<AuthUserData> {
  return {
    ...data,
    getFirstProviderUserId: () => {
      const identities = Object.values(data.identities).filter(isNotNull);
      return identities.length > 0 ? identities[0]?.id ?? null : null;
    },
  };
}

function findUserIdentity(
  user: UserWithAuth,
  providerName: string,
): UserIdentity | null {
  if (!user.auth?.identities) {
    return null;
  }

  return (
    user.auth.identities.find(
      (identity) => identity.providerName === providerName,
    ) ?? null
  );
}

function isNotNull<T>(value: T | null | undefined): value is T {
  return value !== null && value !== undefined;
}
