// Event names and property values are fixed once they are live: a renamed one
// starts again from zero in Plausible and PostHog. Plausible counts an event
// only if a goal with the same name exists, so create the goal first. Never
// send copied text, an email address or an ID as a property value.
export type AnalyticsEvents = {
  // A copy of the Wasp install command, with a copy button or by hand.
  "Install Command: Copy": { method: CopyMethod };
  // A copy of a code block with a `wasp` command.
  "CLI Command: Copy": { method: CopyMethod; command: CliCommand };
  // A copy of an agent plugin command or prompt.
  "Agent Plugin: Copy": { method: CopyMethod; kind: AgentPluginKind };
  // Loops accepted a newsletter signup.
  "Newsletter: Signup": Record<string, never>;
  // Loops rejected a signup, or the request failed.
  "Newsletter: Error": { status: NewsletterErrorStatus };
  // A click on a Discord invite link, or on a `/discord/<source>` link.
  "Discord: Click": Record<string, never>;
  "Get Started: Click": Record<string, never>;
  // A click on a link to `github.com/wasp-lang/...`.
  "GitHub: Click": { target: GitHubTarget };
  // A click on a link to `opensaas.sh` or `docs.opensaas.sh`.
  "Open SaaS: Click": Record<string, never>;
};

export type AnalyticsEventName = keyof AnalyticsEvents;

// Every event also gets a `placement`: where on the page it happened.
const PLACEMENTS = [
  "header",
  "mobile_menu",
  "hero",
  "newsletter",
  // The cards under each blog post.
  "post_card",
  "post_list",
  "sidebar",
  "doc_footer",
  "footer",
  // Anywhere else.
  "body",
] as const;
export type Placement = (typeof PLACEMENTS)[number];

export type CopyMethod = "button" | "selection";

const CLI_COMMANDS = ["wasp_new", "wasp_start"] as const;
export type CliCommand = (typeof CLI_COMMANDS)[number];

const AGENT_PLUGIN_KINDS = [
  "claude_plugin",
  // `npx skills add wasp-lang/wasp-agent-plugins`.
  "skills_cli",
  // A prompt or a slash command for the plugin, for example `/wasp-plugin-init`.
  "plugin_prompt",
  // The example prompt on `/vibe-coding`.
  "example_prompt",
] as const;
export type AgentPluginKind = (typeof AGENT_PLUGIN_KINDS)[number];

// The Loops form endpoint answers 200, 400, 429 or 500.
export type NewsletterErrorStatus =
  // Loops refused the signup, for example for a bad email. Also any other
  // answer that is not a 429, not a 5xx and not a 200 with `success: true`.
  | "400"
  // Too many signups from one IP address in a short time.
  | "429"
  // Any 5xx answer.
  | "500"
  // No answer: `fetch` failed, for example offline or blocked by an extension.
  | "network";

export type GitHubTarget =
  | "repo"
  | "examples"
  | "issues"
  | "edit_page"
  | "other_repo";

export interface TrackOptions {
  leavesPage?: boolean;
}

type DeclaredAction = "copy" | "click";

interface DeclaredEventSpec {
  action: DeclaredAction;
  props: Record<string, readonly string[]>;
}

// The events that the markup declares with `trackEventAttributes()` or
// `<Trackable>`: the action that sends each one, and the allowed values of the
// properties that the markup sets. The listener adds `method` itself.
const DECLARED_EVENTS = {
  "Install Command: Copy": { action: "copy", props: {} },
  "CLI Command: Copy": { action: "copy", props: { command: CLI_COMMANDS } },
  "Agent Plugin: Copy": { action: "copy", props: { kind: AGENT_PLUGIN_KINDS } },
  "Get Started: Click": { action: "click", props: {} },
} as const satisfies {
  [E in AnalyticsEventName]?: {
    action: DeclaredAction;
    // The properties of the event in `AnalyticsEvents`, except `method`.
    props: {
      [P in Exclude<
        keyof AnalyticsEvents[E],
        "method"
      >]: readonly AnalyticsEvents[E][P][];
    };
  };
};

type DeclaredEventName = keyof typeof DECLARED_EVENTS;

type DeclaredProps<E extends DeclaredEventName> = {
  -readonly [P in keyof (typeof DECLARED_EVENTS)[E]["props"]]: (typeof DECLARED_EVENTS)[E]["props"][P] extends readonly (infer Value)[]
    ? Value
    : never;
};

export type DeclaredEvent = {
  [E in DeclaredEventName]: { event: E } & DeclaredProps<E>;
}[DeclaredEventName];

// The markup and the DOM give any string, so look up events by string.
const declaredEventSpecs: Record<string, DeclaredEventSpec | undefined> =
  DECLARED_EVENTS;

const EVENT_ATTRIBUTE = "data-track-event";

// `data-track-placement` on a container sets the placement of every event
// inside it. The nearest one wins.
const PLACEMENT_ATTRIBUTE = "data-track-placement";

export function track<E extends AnalyticsEventName>(
  element: Element,
  eventName: E,
  props: AnalyticsEvents[E],
  options: TrackOptions = {},
): void {
  send(element, eventName, props, options);
}

// Returns the attributes that declare an event on an element. It throws on an
// unknown event or a wrong value. TypeScript does not check MDX and `.jsx`
// files, and `docusaurus build` renders every page, so the build fails.
export function trackEventAttributes(
  declared: DeclaredEvent,
): Record<string, string> {
  const { event, ...props }: { event: string } & Record<string, unknown> =
    declared;
  const spec = declaredEventSpecs[event];
  if (!spec) {
    throw new Error(`Unknown analytics event "${event}".`);
  }

  for (const name of Object.keys(props)) {
    if (!(name in spec.props)) {
      throw new Error(`"${event}" has no property "${name}".`);
    }
  }

  const attributes: Record<string, string> = { [EVENT_ATTRIBUTE]: event };
  for (const [name, allowedValues] of Object.entries(spec.props)) {
    const value = props[name];
    if (typeof value !== "string" || !allowedValues.includes(value)) {
      throw new Error(
        `"${event}" needs "${name}" to be one of: ${allowedValues.join(", ")}. It got: ${String(value)}.`,
      );
    }
    attributes[propAttribute(name)] = value;
  }
  return attributes;
}

export function trackDeclaredClick(element: Element): void {
  trackDeclaredEvent(element, "click", {});
}

export function trackDeclaredCopy(element: Element, method: CopyMethod): void {
  trackDeclaredEvent(element, "copy", { method });
}

// Sends the event that the nearest `data-track-event` declares, if that event
// belongs to this action.
function trackDeclaredEvent(
  element: Element,
  action: DeclaredAction,
  actionProps: Record<string, string>,
): void {
  const declaredElement = element.closest(`[${EVENT_ATTRIBUTE}]`);
  const event = declaredElement?.getAttribute(EVENT_ATTRIBUTE);
  if (!declaredElement || !event) {
    return;
  }

  const spec = declaredEventSpecs[event];
  if (spec?.action !== action) {
    return;
  }

  const props: Record<string, string> = {};
  for (const [name, allowedValues] of Object.entries(spec.props)) {
    const value = declaredElement.getAttribute(propAttribute(name));
    if (value === null || !allowedValues.includes(value)) {
      return;
    }
    props[name] = value;
  }

  send(declaredElement, event, { ...props, ...actionProps });
}

// Returns the attribute that sets the placement of every event inside an
// element. It throws on an unknown placement, so a typo fails the build, as
// in `trackEventAttributes()`.
export function trackPlacementAttributes(
  placement: Placement,
): Record<string, string> {
  if (!isPlacement(placement)) {
    throw new Error(
      `Unknown analytics placement "${placement}". Use one of: ${PLACEMENTS.join(", ")}.`,
    );
  }
  return { [PLACEMENT_ATTRIBUTE]: placement };
}

export function getPlacement(element: Element): Placement {
  const placement = element
    .closest(`[${PLACEMENT_ATTRIBUTE}]`)
    ?.getAttribute(PLACEMENT_ATTRIBUTE);
  return placement && isPlacement(placement) ? placement : "body";
}

function isPlacement(value: string): value is Placement {
  return (PLACEMENTS as readonly string[]).includes(value);
}

function send(
  element: Element,
  eventName: string,
  props: Record<string, string>,
  options: TrackOptions = {},
): void {
  const allProps: Record<string, string> = {
    ...props,
    placement: getPlacement(element),
  };

  // Plausible counts a visit as a bounce if it has one pageview and no
  // interactive event. A custom event is interactive by default, so a copy or
  // a click would turn a bounce into a non-bounce. `interactive: false` keeps
  // these events out of the Plausible bounce rate, so the bounce rate stays
  // comparable with the months before these events. Plausible still counts
  // the events themselves.
  window.plausible?.(eventName, { props: allProps, interactive: false });
  window.posthog?.capture?.(
    eventName,
    allProps,
    options.leavesPage ? { transport: "sendBeacon" } : undefined,
  );
}

function propAttribute(name: string): string {
  return `data-track-${name}`;
}
