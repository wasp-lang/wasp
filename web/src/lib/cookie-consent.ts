// A link with this `href` opens the cookie banner again, so visitors can change
// their choice. `src/components/CookieConsentBanner.tsx` handles the click.
// The `href` marks the link, so the link needs no second attribute. Both
// footers take it from this constant.
export const COOKIE_SETTINGS_HREF = "#cookie-settings";

/**
 * Returns `true` when the visitor must accept cookies before PostHog records
 * anything. Any failure returns `true`, so a visitor with an unknown region
 * gets the cookie banner.
 *
 * Only Cloudflare runs the endpoint (`functions/api/requires-consent.ts`).
 * Locally the answer is the HTML of the site, not JSON, so this function
 * returns `true` and every visitor gets the banner.
 */
export async function fetchRequiresConsent(): Promise<boolean> {
  try {
    // The answer depends on the country of each request, so the browser must
    // not take it from its cache.
    const response = await fetch("/api/requires-consent", {
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
