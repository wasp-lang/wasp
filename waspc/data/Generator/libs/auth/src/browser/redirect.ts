/**
 * Context passed to the `onAuthSucceededRedirect` function from the app's
 * auth config.
 */
export type OnAuthSucceededRedirectContext = {
  /**
   * Client route (path + query params + hash) the user tried to visit before
   * being redirected to the login page, e.g. `/tasks/42?tab=details#comments`.
   * `undefined` when the user went to the login page directly.
   */
  originalRoute: string | undefined;
};

/**
 * Decides where the user is redirected after a successful login or signup.
 */
export type OnAuthSucceededRedirectFn = (
  context: OnAuthSucceededRedirectContext,
) => string;

/**
 * After a successful login, redirects the user to the route they originally
 * tried to visit, falling back to `options.fallback` (default `/`) if they
 * went to the login page directly.
 */
export function redirectToOriginalRoute(options?: {
  fallback?: string;
}): OnAuthSucceededRedirectFn {
  const fallback = options?.fallback ?? "/";
  return ({ originalRoute }) => originalRoute ?? fallback;
}

/**
 * After a successful login, always redirects the user to the given route.
 */
export function redirectToFixed(route: string): OnAuthSucceededRedirectFn {
  return () => route;
}

const ORIGINAL_ROUTE_STORAGE_KEY = "wasp:auth:original-route";

export function saveOriginalRoute(route: string): void {
  if (!isSafeInternalRoute(route)) {
    return;
  }
  try {
    getSessionStorage()?.setItem(ORIGINAL_ROUTE_STORAGE_KEY, route);
  } catch {
    // Storage can be unavailable (e.g. disabled by browser settings). Losing
    // the original route only degrades to the fallback redirect.
  }
}

/**
 * Reads the saved original route without clearing it, so it is safe to call
 * multiple times (e.g. from React render code).
 */
export function peekOriginalRoute(): string | undefined {
  let route: string | null;
  try {
    route = getSessionStorage()?.getItem(ORIGINAL_ROUTE_STORAGE_KEY) ?? null;
  } catch {
    return undefined;
  }
  // Anything can write to sessionStorage, so we must validate the route
  // again when reading it.
  return route !== null && isSafeInternalRoute(route) ? route : undefined;
}

export function clearOriginalRoute(): void {
  try {
    getSessionStorage()?.removeItem(ORIGINAL_ROUTE_STORAGE_KEY);
  } catch {
    // Nothing to clear if storage is unavailable.
  }
}

export function consumeOriginalRoute(): string | undefined {
  const route = peekOriginalRoute();
  clearOriginalRoute();
  return route;
}

/**
 * Only client routes within the app are safe redirect targets: absolute URLs
 * (`https://...`), protocol-relative URLs (`//...`, `/\...`) and scheme URLs
 * (`javascript:...`) would allow open redirects.
 */
function isSafeInternalRoute(route: string): boolean {
  return /^\/(?![/\\])/.test(route);
}

function getSessionStorage(): Storage | undefined {
  // The `typeof` check keeps this safe in non-browser environments (SSR).
  return typeof sessionStorage === "undefined" ? undefined : sessionStorage;
}
