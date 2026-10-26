import { Cookies } from "react-cookie-consent";

export const SCRIPT_WITH_CONSENT_TYPE = "text/javascript+requires-consent";

export const CONSENT_COOKIE_NAME = "wasp-cookies-consent";

// A link with this `href` opens the cookie banner again, so visitors can change
// their choice. `src/components/CookieConsentBanner.tsx` handles the click.
// The `href` marks the link, so the link needs no second attribute. Both
// footers take it from this constant.
export const COOKIE_SETTINGS_HREF = "#cookie-settings";

// Only Cloudflare runs this endpoint (`functions/api/requires-consent.ts`).
// Locally the answer is the HTML of the site, not JSON, so
// `fetchRequiresConsent()` returns `true` and every visitor gets the banner.
const REQUIRES_CONSENT_ENDPOINT = "/api/requires-consent";

// PostHog keeps the visitor ID and its other data under names with this
// prefix, in cookies, localStorage and sessionStorage. Its opt-out record has
// the prefix `__ph_opt_in_out_`, so that record stays.
const POSTHOG_STORAGE_PREFIX = "ph_";

type ConsentChoice = NonNullable<Window["waspCookieConsent"]>;

/**
 * Returns the latest choice of the visitor, made in any tab, and `undefined`
 * when there is no choice. The cookie comes first, because another tab can
 * change it. `window.waspCookieConsent` has the choice of this page when the
 * browser blocks cookies.
 */
export function getConsent(): ConsentChoice | undefined {
  const choice = Cookies.get(CONSENT_COOKIE_NAME) ?? window.waspCookieConsent;
  return choice === "true" || choice === "false" ? choice : undefined;
}

/**
 * Deletes the PostHog data from the browser. A loaded PostHog does this itself
 * at `opt_out_capturing()`. This function also covers a PostHog that did not
 * load, for example because an ad blocker stopped its script.
 */
export function deletePostHogStorage(): void {
  const hostnameParts = window.location.hostname.split(".");
  for (const name of Object.keys(Cookies.get())) {
    if (!name.startsWith(POSTHOG_STORAGE_PREFIX)) {
      continue;
    }
    Cookies.remove(name, { path: "/" });
    // PostHog sets its cookie on the main domain of the site, for example
    // `.wasp.sh`, so remove the cookie on each parent domain too.
    hostnameParts.forEach((_, index) => {
      Cookies.remove(name, {
        path: "/",
        domain: hostnameParts.slice(index).join("."),
      });
    });
  }

  try {
    for (const storage of [window.localStorage, window.sessionStorage]) {
      Object.keys(storage)
        .filter((key) => key.startsWith(POSTHOG_STORAGE_PREFIX))
        .forEach((key) => storage.removeItem(key));
    }
  } catch {
    // The browser blocks the storage, so PostHog could not use it either.
  }
}

/**
 * Returns `true` when the visitor must accept cookies before PostHog starts.
 * Any failure returns `true`, so a visitor with an unknown region gets the
 * cookie banner.
 */
export async function fetchRequiresConsent(): Promise<boolean> {
  try {
    const response = await fetch(REQUIRES_CONSENT_ENDPOINT, {
      cache: "no-store",
    });
    if (!response.ok) {
      return true;
    }
    const body = (await response.json()) as { requiresConsent?: unknown };
    return body.requiresConsent !== false;
  } catch {
    return true;
  }
}
