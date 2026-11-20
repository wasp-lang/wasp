// A button with this attribute opens the cookie banner again, so visitors can
// change their choice. `src/components/CookieConsentBanner.tsx` handles the
// click. Both footers add the attribute from this constant.
export const COOKIE_SETTINGS_ATTRIBUTE = "data-cookie-settings";

// The answer of `functions/api/requires-consent.ts`.
export interface RequiresConsentResponse {
  requiresConsent: boolean;
}

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
    const body = (await response.json()) as Partial<RequiresConsentResponse>;
    return body.requiresConsent !== false;
  } catch {
    return true;
  }
}
