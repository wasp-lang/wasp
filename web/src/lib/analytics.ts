import type { AgentPluginKind, CliCommand } from "./analytics-classifier";

// Event names and property values are fixed once they are live: a renamed one
// starts again from zero in Plausible and PostHog. Plausible counts an event
// only if a goal with the same name exists, so create the goal first. Never
// send copied text, an email address or an ID as a property value.
export type AnalyticsEvents = {
  // A copy of `npm i -g @wasp.sh/wasp-cli`, with a copy button or by hand.
  "Install Command: Copy": { method: CopyMethod };
  // A copy from any other shell code block.
  "CLI Command: Copy": { method: CopyMethod; command: CliCommand };
  // A copy of an agent plugin command or prompt.
  "Agent Plugin: Copy": { method: CopyMethod; kind: AgentPluginKind };
  // Loops accepted a newsletter signup.
  "Newsletter: Signup": Record<string, never>;
  // Loops rejected a signup, or the request failed.
  "Newsletter: Error": { status: NewsletterErrorStatus };
  // A click on a Discord invite link, or on a `/discord/<source>` link.
  "Discord: Click": Record<string, never>;
  // A click on an element with `data-track-event="Get Started: Click"`.
  "Get Started: Click": Record<string, never>;
  // A click on a link to `github.com/wasp-lang/...`.
  "GitHub: Click": { target: GitHubTarget };
  // A click on a link to `opensaas.sh` or `docs.opensaas.sh`.
  "Open SaaS: Click": Record<string, never>;
};

export type AnalyticsEventName = keyof AnalyticsEvents;

// Every event also gets a `placement`: where on the page it happened.
export type Placement =
  | "announcement_bar"
  | "header"
  | "mobile_menu"
  | "hero"
  // The newsletter section of the homepage.
  | "newsletter"
  // The cards under each blog post.
  | "post_card"
  | "post_list"
  | "sidebar"
  | "toc"
  | "doc_footer"
  | "footer"
  // Anywhere else.
  | "body";

export type CopyMethod = "button" | "selection";

export type NewsletterErrorStatus = "400" | "429" | "500" | "network";

export type GitHubTarget =
  | "repo"
  | "examples"
  | "issues"
  | "edit_page"
  | "other_repo";

export interface TrackOptions {
  leavesPage?: boolean;
}

// `data-track-placement` on a container sets the placement of every event
// inside it. The nearest one wins.
const PLACEMENT_ATTRIBUTE = "data-track-placement";

// Checked first: the Docusaurus mobile menu reuses the navbar item props.
const MOBILE_MENU_SELECTOR = ".navbar-sidebar, .theme-layout-navbar-sidebar";

const PLACEMENT_BY_THEME_SELECTOR: [selector: string, placement: Placement][] =
  [
    [".theme-announcement-bar", "announcement_bar"],
    [".theme-layout-navbar, .navbar", "header"],
    [".theme-doc-sidebar-container", "sidebar"],
    [".theme-doc-toc-desktop, .theme-doc-toc-mobile", "toc"],
    [".theme-doc-footer", "doc_footer"],
    [".theme-layout-footer, .footer", "footer"],
  ];

type PlausibleFn = (
  eventName: string,
  options?: { props?: Record<string, string>; interactive?: boolean },
) => void;

interface PostHogLike {
  capture?: (
    eventName: string,
    properties?: Record<string, string>,
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

export function track<E extends AnalyticsEventName>(
  element: Element,
  eventName: E,
  props: AnalyticsEvents[E],
  options: TrackOptions = {},
): void {
  if (typeof window === "undefined") {
    return;
  }

  const allProps: Record<string, string> = {
    ...props,
    placement: getPlacement(element),
  };

  if (isDebugEnabled()) {
    console.info("[analytics]", eventName, allProps);
  }

  // `interactive: false` keeps these events out of the bounce rate.
  window.plausible?.(eventName, { props: allProps, interactive: false });
  window.posthog?.capture?.(
    eventName,
    allProps,
    options.leavesPage ? { transport: "sendBeacon" } : undefined,
  );
}

export function getPlacement(element: Element): Placement {
  if (element.closest(MOBILE_MENU_SELECTOR)) {
    return "mobile_menu";
  }

  const ownPlacement = element
    .closest(`[${PLACEMENT_ATTRIBUTE}]`)
    ?.getAttribute(PLACEMENT_ATTRIBUTE);
  if (ownPlacement) {
    return ownPlacement as Placement;
  }

  for (const [selector, placement] of PLACEMENT_BY_THEME_SELECTOR) {
    if (element.closest(selector)) {
      return placement;
    }
  }

  return "body";
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
