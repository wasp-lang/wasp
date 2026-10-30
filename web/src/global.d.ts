// Set by the Plausible and PostHog scripts in `getScripts()` in
// `docusaurus.config.ts`.
interface Window {
  plausible?: (
    eventName: string,
    options?: { props?: Record<string, string>; interactive?: boolean },
  ) => void;
  posthog?: {
    capture?: (
      eventName: string,
      properties?: Record<string, string>,
      options?: { transport?: "sendBeacon" },
    ) => void;
    has_opted_out_capturing?: () => boolean;
    opt_in_capturing?: (options?: { captureEventName?: false }) => void;
    opt_out_capturing?: () => void;
  };
  // Set by `src/components/CookieConsentBanner.tsx`: the choice made on this
  // page, or a "Reject all" from another tab. It works also when the browser
  // blocks cookies. `static/scripts/posthog.js` reads it too.
  waspCookieConsent?: "true" | "false";
}
