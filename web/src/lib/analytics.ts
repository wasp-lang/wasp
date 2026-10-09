export type AnalyticsEventName =
  | "Install Command: Copy"
  | "CLI Command: Copy"
  | "Agent Plugin: Copy"
  | "Newsletter: Signup"
  | "Newsletter: Error"
  | "Discord: Click"
  | "Get Started: Click"
  | "GitHub: Click"
  | "Open SaaS: Click";

export type AnalyticsProps = Record<string, string>;

export interface TrackOptions {
  leavesPage?: boolean;
}

type PlausibleFn = ((
  eventName: string,
  options?: { props?: AnalyticsProps; interactive?: boolean },
) => void) & { q?: unknown[] };

interface PostHogLike {
  capture?: (
    eventName: string,
    properties?: AnalyticsProps,
    options?: { transport?: "sendBeacon" },
  ) => void;
}

declare global {
  interface Window {
    plausible?: PlausibleFn;
    posthog?: PostHogLike;
  }
}

const DEBUG_STORAGE_KEY = "wasp-analytics-debug";

export function track(
  eventName: AnalyticsEventName,
  props: AnalyticsProps = {},
  options: TrackOptions = {},
): void {
  if (typeof window === "undefined") {
    return;
  }

  if (isDebugEnabled()) {
    console.info("[analytics]", eventName, props);
  }

  sendToPlausible(eventName, props);
  sendToPostHog(eventName, props, options);
}

function sendToPlausible(eventName: string, props: AnalyticsProps): void {
  // Queue events until the deferred Plausible script loads and sends them.
  if (!window.plausible) {
    const queue: unknown[] = [];
    const queueEvent: PlausibleFn = (...args) => {
      queue.push(args);
    };
    queueEvent.q = queue;
    window.plausible = queueEvent;
  }

  // `interactive: false` keeps these events out of the bounce rate.
  window.plausible(eventName, { props, interactive: false });
}

function sendToPostHog(
  eventName: string,
  props: AnalyticsProps,
  { leavesPage }: TrackOptions,
): void {
  window.posthog?.capture?.(
    eventName,
    props,
    leavesPage ? { transport: "sendBeacon" } : undefined,
  );
}

function isDebugEnabled(): boolean {
  if (process.env.NODE_ENV !== "production") {
    return true;
  }

  try {
    return window.localStorage.getItem(DEBUG_STORAGE_KEY) === "true";
  } catch {
    return false;
  }
}
