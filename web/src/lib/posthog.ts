import type { PostHog } from "posthog-js";

// PostHog keeps the choice of the visitor in localStorage under this name, as
// `1` or `0`.
export const CONSENT_STORAGE_KEY = "wasp-cookies-consent";

type CaptureArguments = Parameters<PostHog["capture"]>;

let startedPostHog: PostHog | undefined;

// Events from before PostHog starts wait here, as Plausible events wait in
// `static/scripts/plausible-queue.js`. At the start, PostHog gets them and
// applies the choice of the visitor to them.
let eventsBeforeStart: CaptureArguments[] = [];

export function captureInPostHog(...event: CaptureArguments): void {
  if (startedPostHog) {
    startedPostHog.capture(...event);
  } else {
    eventsBeforeStart.push(event);
  }
}

/**
 * Loads PostHog from its own chunk of the site bundle and starts it. With
 * `requiresConsent`, PostHog sends nothing and stores nothing in the browser
 * until the visitor clicks "Accept all".
 */
export async function startPostHog(requiresConsent: boolean): Promise<PostHog> {
  const earlierChoice = copyChoiceFromEarlierBanner();

  // The `no-external` build of PostHog contains no code that downloads other
  // scripts. The npm lockfile fixes its version.
  const { default: posthog } = await import("posthog-js/no-external");

  posthog.init("CdDd2A0jKTI2vFAsrI9JWm3MqpOcgHz1bMyogAcwsE4", {
    api_host: "https://us.i.posthog.com",
    person_profiles: "identified_only",
    // Docusaurus uses client-side navigation, so page changes don't trigger a
    // full page load. This tells PostHog to capture a page view on each path
    // change.
    capture_pageview: "history_change",
    // A second guard, in addition to the `no-external` build.
    disable_external_dependency_loading: true,
    // In the consent region, the visitor starts opted out. A stored choice
    // counts before this default. A "Reject all" in the earlier banner also
    // counts when the browser could not copy it into localStorage.
    opt_out_capturing_by_default: requiresConsent || earlierChoice === "false",
    // While the visitor is opted out, PostHog stores nothing in the browser.
    // An opt-out also deletes the PostHog cookie and localStorage data.
    opt_out_persistence_by_default: true,
    consent_persistence_name: CONSENT_STORAGE_KEY,
    // A browser with Do Not Track or Global Privacy Control counts as "Reject
    // all", so the banner does not show there.
    respect_dnt: true,
    // The privacy policy does not name these features. These lines keep them
    // off, also when somebody turns them on in the PostHog project.
    disable_session_recording: true,
    disable_surveys: true,
    capture_dead_clicks: false,
    capture_exceptions: false,
    capture_performance: false,
    // The site reads no feature flags and no settings from the PostHog
    // project. Without this line, PostHog asks its server for them, also
    // before a choice in the consent region.
    advanced_disable_flags: true,
    // `syncHeatmaps()` turns them on.
    capture_heatmaps: false,
  });
  syncHeatmaps(posthog);

  startedPostHog = posthog;
  eventsBeforeStart.forEach((event) => posthog.capture(...event));
  eventsBeforeStart = [];
  return posthog;
}

export function optInToPostHog(posthog: PostHog): void {
  // PostHog also captures the page view of this page now. The "Cookie
  // Consent: Accept" event counts the choice, so PostHog sends no `$opt_in`
  // event.
  posthog.opt_in_capturing({ captureEventName: false });
  syncHeatmaps(posthog);
}

// This also deletes the PostHog cookie and localStorage data.
export function optOutOfPostHog(posthog: PostHog): void {
  posthog.opt_out_capturing();
  syncHeatmaps(posthog);
}

// PostHog collects heatmap clicks and mouse moves in memory also while the
// visitor is opted out, and sends them after an opt-in. So heatmaps run only
// while PostHog captures. Without the settings of the PostHog project, only
// `capture_heatmaps` turns them on.
function syncHeatmaps(posthog: PostHog): void {
  posthog.set_config({ capture_heatmaps: posthog.is_capturing() });
}

// The earlier banner kept the choice in a cookie with the same name, as `true`
// or `false`. PostHog reads these values too, so this function copies the
// cookie into localStorage before `init`, and a "Reject all" from the earlier
// banner still counts. It returns the choice from the cookie. The cookie lasts
// one year, so one year after this change is live, this function can go.
function copyChoiceFromEarlierBanner(): "true" | "false" | undefined {
  const earlierChoice = document.cookie
    .split("; ")
    .find((cookie) => cookie.startsWith(`${CONSENT_STORAGE_KEY}=`))
    ?.slice(CONSENT_STORAGE_KEY.length + 1);
  if (earlierChoice !== "true" && earlierChoice !== "false") {
    return undefined;
  }

  try {
    if (localStorage.getItem(CONSENT_STORAGE_KEY) === null) {
      localStorage.setItem(CONSENT_STORAGE_KEY, earlierChoice);
    }
  } catch {
    // The browser blocks localStorage, so PostHog keeps the choice only in
    // memory. The cookie stays for a later visit.
    return earlierChoice;
  }
  document.cookie = `${CONSENT_STORAGE_KEY}=; path=/; max-age=0`;
  return earlierChoice;
}
