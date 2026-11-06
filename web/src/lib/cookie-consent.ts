// A link with this `href` opens the cookie banner again, so visitors can change
// their choice. `src/components/CookieConsentBanner.tsx` handles the click.
// The `href` marks the link, so the link needs no second attribute. Both
// footers take it from this constant.
export const COOKIE_SETTINGS_HREF = "#cookie-settings";

/**
 * Asks `functions/api/requires-consent.ts` if the law in the country of the
 * visitor requires consent for analytics. Any failure counts as `true`, so the
 * visitor gets the cookie banner.
 */
export async function isAnalyticsConsentRequired(): Promise<boolean> {
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
