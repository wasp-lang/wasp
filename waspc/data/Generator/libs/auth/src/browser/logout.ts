import {
  invalidateAndRemoveQueries,
  removeLocalUserData,
} from "@wasp.sh/lib-sdk-core/browser";
import type { KyInstance } from "ky";

/** Creates the `logout` function of the app. */
export function createLogout({
  api,
}: {
  api: KyInstance;
}): () => Promise<void> {
  return async function logout(): Promise<void> {
    try {
      await api.post("/auth/logout");
    } finally {
      // Even if the logout request fails, we still want to remove the local user data
      // in case the logout failed because of a network error and the user walked away
      // from the computer.
      removeLocalUserData();

      // TODO(filip): We are currently invalidating and removing  all the queries, but
      // we should remove only the non-public, user-dependent ones.
      await invalidateAndRemoveQueries();
    }
  };
}
