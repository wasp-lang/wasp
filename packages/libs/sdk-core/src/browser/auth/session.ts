import { isHTTPError } from "ky";
import { fromHttpErrorBody } from "../../HttpError.js";
import { storage } from "../storage.js";
import { apiEventsEmitter } from "./events.js";

const WASP_APP_AUTH_SESSION_ID_NAME = "sessionId";

export function setSessionId(sessionId: string): void {
  storage.set(WASP_APP_AUTH_SESSION_ID_NAME, sessionId);
  apiEventsEmitter.emit("sessionId.set");
}

export function getSessionId(): string | null {
  const sessionId = storage.get(WASP_APP_AUTH_SESSION_ID_NAME) as
    | string
    | undefined;
  return sessionId ?? null;
}
export function clearSessionId(): void {
  storage.remove(WASP_APP_AUTH_SESSION_ID_NAME);
  apiEventsEmitter.emit("sessionId.clear");
}

export function removeLocalUserData(): void {
  storage.clear();
  apiEventsEmitter.emit("sessionId.clear");
}

// This makes sure that the following handler won't try to run in a non-browser
// environment (e.g. during SSR), where `window` is not defined.
if (typeof window !== "undefined") {
  // This handler will run on other tabs (not the active one calling API functions),
  // and will ensure they know about auth session ID changes.
  // Ref: https://developer.mozilla.org/en-US/docs/Web/API/Window/storage_event
  // "Note: This won't work on the same page that is making the changes — it is really a way
  // for other pages on the domain using the storage to sync any changes that are made."
  window.addEventListener("storage", (event) => {
    if (event.key === storage.getPrefixedKey(WASP_APP_AUTH_SESSION_ID_NAME)) {
      if (!!event.newValue) {
        apiEventsEmitter.emit("sessionId.set");
      } else {
        apiEventsEmitter.emit("sessionId.clear");
      }
    }
  });
}

/**
 * Takes an error thrown by ky and turns an HTTP error response into the
 * same `HttpError` the server threw. Any other error, e.g. a network
 * failure, is returned unchanged.
 */
export function handleApiError(error: unknown): unknown {
  if (isHTTPError(error)) {
    return fromHttpErrorBody({
      statusCode: error.response.status,
      body: error.data,
      fallbackMessage: error.message,
    });
  }
  return error;
}

import { invalidateAndRemoveQueries } from "../operations/internal/resources.js";

export async function initSession(sessionId: string): Promise<void> {
  setSessionId(sessionId);
  // We need to invalidate queries after login in order to get the correct user
  // data in the React components (using `useAuth`).
  // Redirects after login won't work properly without this.

  // TODO(filip): We are currently removing all the queries, but we should
  // remove only non-public, user-dependent queries - public queries are
  // expected not to change in respect to the currently logged in user.
  await invalidateAndRemoveQueries();
}
