import type { UseQueryResult } from "@tanstack/react-query";
import { createAuthQuery } from "@wasp.sh/lib-auth/browser";
import type { Query } from "@wasp.sh/lib-sdk-core";
import { api } from "../api/index.js";
import { deserialize } from "../core/serialization/index.js";
import type { AuthUser, AuthUserData } from "./user.js";

const authQuery = createAuthQuery<AuthUserData>({
  api,
  deserialize,
  entitiesGetMeDependsOn: ["User"],
});

// PUBLIC API
export const getMe: Query<void, AuthUser | null> = authQuery.getMe;

// PUBLIC API
export default function useAuth(): UseQueryResult<AuthUser | null> {
  return authQuery.useAuth();
}
