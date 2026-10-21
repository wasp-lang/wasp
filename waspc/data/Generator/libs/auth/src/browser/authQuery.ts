import type { UseQueryResult } from "@tanstack/react-query";
import {
  HttpMethod,
  type Query,
  type QueryFunction,
} from "@wasp.sh/lib-sdk-core";
import {
  buildAndRegisterQuery,
  handleApiError,
  useQuery,
} from "@wasp.sh/lib-sdk-core/browser";
import type { KyInstance } from "ky";
import { makeAuthUserIfPossible, type AuthUserMethods } from "../shared/authUser.js";

export type AuthQueryConfig = {
  /** The app's API client. */
  api: KyInstance;
  /**
   * The app's response deserializer (superjson with the app's registered custom
   * types). Takes the parsed JSON body and returns the deserialized value.
   */
  deserialize: (json: any) => unknown;
  /** Entities whose changes invalidate the current user query. */
  entitiesGetMeDependsOn: string[];
};

export type AuthQuery<AuthUser> = {
  getMe: Query<void, AuthUser | null>;
  useAuth: () => UseQueryResult<AuthUser | null>;
};

/**
 * Creates the `getMe` query and the `useAuth` hook for the current user.
 */
export function createAuthQuery<
  AuthUserData extends {
    identities: { [providerName: string]: { id: string } | null };
  },
>(config: AuthQueryConfig): AuthQuery<AuthUserData & AuthUserMethods> {
  type AuthUser = AuthUserData & AuthUserMethods;

  const getMeRelativePath = "auth/me";
  const getMeRoute = { method: HttpMethod.Get, path: `/${getMeRelativePath}` };
  const getMeFn: QueryFunction<void, AuthUser | null> = async () => {
    try {
      const json = await config.api.get(getMeRoute.path).json();
      const userData = config.deserialize(json) as AuthUserData | null;
      return makeAuthUserIfPossible(userData);
    } catch (error) {
      throw handleApiError(error);
    }
  };

  const getMe: Query<void, AuthUser | null> = buildAndRegisterQuery(getMeFn, {
    queryCacheKey: [getMeRelativePath],
    queryRoute: getMeRoute,
    entitiesUsed: config.entitiesGetMeDependsOn,
  });

  function useAuth(): UseQueryResult<AuthUser | null> {
    return useQuery(getMe);
  }

  return { getMe, useAuth };
}
