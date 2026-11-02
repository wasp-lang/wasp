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
  };
}
