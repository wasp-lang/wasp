// Cloudflare adds the country of the visitor to each request, in `cf`.
type RequestWithCountry = Request & { cf?: { country?: string } };

// Visitors from these countries must accept cookies before PostHog starts:
// the EU (with its regions that have their own country code), the EEA, the
// United Kingdom and Switzerland.
const CONSENT_COUNTRY_CODES = new Set([
  // EU
  "AT",
  "BE",
  "BG",
  "CY",
  "CZ",
  "DE",
  "DK",
  "EE",
  "ES",
  "FI",
  "FR",
  "GR",
  "HR",
  "HU",
  "IE",
  "IT",
  "LT",
  "LU",
  "LV",
  "MT",
  "NL",
  "PL",
  "PT",
  "RO",
  "SE",
  "SI",
  "SK",
  // EU regions
  "AX",
  "GF",
  "GP",
  "MF",
  "MQ",
  "RE",
  "YT",
  // EEA
  "IS",
  "LI",
  "NO",
  // The United Kingdom and Switzerland
  "GB",
  "CH",
]);

// Cloudflare uses `XX` when it does not know the country, and `T1` for Tor.
const UNKNOWN_COUNTRY_CODES = new Set(["XX", "T1"]);

/**
 * Tells the page whether the visitor must accept cookies before PostHog
 * starts. The page asks once per full page load, and shows the cookie banner
 * when the answer is `true` or when the request fails.
 *
 * Cloudflare Pages runs this file for `GET /api/requires-consent`, because the
 * path of the file is the route and `static/_routes.json` includes it.
 *
 * The answer depends on the country of each request, so it must never be
 * cached.
 */
export const onRequestGet = async (context: {
  request: RequestWithCountry;
}): Promise<Response> => {
  const countryCode = context.request.cf?.country;
  const requiresConsent =
    !countryCode ||
    UNKNOWN_COUNTRY_CODES.has(countryCode) ||
    CONSENT_COUNTRY_CODES.has(countryCode);

  return new Response(JSON.stringify({ requiresConsent }), {
    headers: {
      "Content-Type": "application/json",
      "Cache-Control": "no-store",
    },
  });
};
