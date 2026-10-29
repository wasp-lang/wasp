// The markup declares analytics events with `data-track-*` attributes.
// `data-track-event` names the event of an element, and each property that the
// markup sets has its own `data-track-<property>` attribute.
// `data-track-placement` on a container sets the placement of every event
// inside it. The nearest one wins.
const EVENT_ATTRIBUTE = trackAttributeName("event");
const PLACEMENT_ATTRIBUTE = trackAttributeName("placement");

function trackAttributeName(name: string): string {
  return `data-track-${name}`;
}

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
  // Too many signups from one IP address in a short time.
  | "429"
  // Any 5xx answer: an error on the Loops side.
  | "5xx"
  // No answer: `fetch` failed, for example offline or blocked by an extension.
  | "network"
  // Any other answer without `success: true`, for example a 400 for a bad
  // email.
  | "other";

export type GitHubTarget =
  | "repo"
  | "examples"
  | "issues"
  | "edit_page"
  | "other_repo";

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

export interface TrackOptions {
  leavesPage?: boolean;
}

type DeclaredAction = "copy" | "click";

// What the markup may declare for an event: the action that sends it, and the
// allowed values of each property that the markup sets. The listener adds
// `method` itself, so the markup never sets it.
type DeclaredEventSpec<E extends AnalyticsEventName> = {
  action: DeclaredAction;
  props: {
    [P in Exclude<
      keyof AnalyticsEvents[E],
      "method"
    >]: readonly AnalyticsEvents[E][P][];
  };
};

// The events that the markup declares with `trackEventAttributes()` or
// `<Trackable>`.
const DECLARED_EVENTS = {
  "Install Command: Copy": { action: "copy", props: {} },
  "CLI Command: Copy": { action: "copy", props: { command: CLI_COMMANDS } },
  "Agent Plugin: Copy": { action: "copy", props: { kind: AGENT_PLUGIN_KINDS } },
  "Get Started: Click": { action: "click", props: {} },
} as const satisfies { [E in AnalyticsEventName]?: DeclaredEventSpec<E> };

type DeclaredEventName = keyof typeof DECLARED_EVENTS;

type AllowedPropValues<E extends DeclaredEventName> =
  (typeof DECLARED_EVENTS)[E]["props"];

type ItemOf<List> = List extends readonly (infer Item)[] ? Item : never;

type DeclaredProps<E extends DeclaredEventName> = {
  -readonly [P in keyof AllowedPropValues<E>]: ItemOf<AllowedPropValues<E>[P]>;
};

export type DeclaredEvent = {
  [E in DeclaredEventName]: { event: E } & DeclaredProps<E>;
}[DeclaredEventName];

// The markup and the DOM give any string, so the code looks events up by
// string.
type AnyDeclaredEventSpec = {
  action: DeclaredAction;
  props: Record<string, readonly string[]>;
};
const declaredEventSpecs: Record<string, AnyDeclaredEventSpec | undefined> =
  DECLARED_EVENTS;

type FoundDeclaredEvent = {
  element: Element;
  event: string;
  props: Record<string, string>;
};

export function track<E extends AnalyticsEventName>(
  element: Element,
  eventName: E,
  props: AnalyticsEvents[E],
  options: TrackOptions = {},
): void {
  sendEvent(element, eventName, props, options);
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

  const unknownProp = Object.keys(props).find((name) => !(name in spec.props));
  if (unknownProp) {
    throw new Error(`"${event}" has no property "${unknownProp}".`);
  }

  const attributes: Record<string, string> = { [EVENT_ATTRIBUTE]: event };
  for (const [name, allowedValues] of Object.entries(spec.props)) {
    attributes[trackAttributeName(name)] = requireAllowedValue(
      event,
      name,
      props[name],
      allowedValues,
    );
  }
  return attributes;
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

export function trackDeclaredClick(element: Element): void {
  const declared = findDeclaredEvent(element, "click");
  if (declared) {
    sendEvent(declared.element, declared.event, declared.props);
  }
}

export function trackDeclaredCopy(element: Element, method: CopyMethod): void {
  const declared = findDeclaredEvent(element, "copy");
  if (declared) {
    sendEvent(declared.element, declared.event, { ...declared.props, method });
  }
}

export function getPlacement(element: Element): Placement {
  const placement = element
    .closest(`[${PLACEMENT_ATTRIBUTE}]`)
    ?.getAttribute(PLACEMENT_ATTRIBUTE);
  return placement && isPlacement(placement) ? placement : "body";
}

// Reads the event that the nearest `data-track-event` declares. Returns null if
// that event does not belong to this action, or if a property value is not
// allowed.
function findDeclaredEvent(
  element: Element,
  action: DeclaredAction,
): FoundDeclaredEvent | null {
  const declaredElement = element.closest(`[${EVENT_ATTRIBUTE}]`);
  const event = declaredElement?.getAttribute(EVENT_ATTRIBUTE);
  const spec = event ? declaredEventSpecs[event] : undefined;
  if (!declaredElement || !event || spec?.action !== action) {
    return null;
  }

  const props: Record<string, string> = {};
  for (const [name, allowedValues] of Object.entries(spec.props)) {
    const value = declaredElement.getAttribute(trackAttributeName(name));
    if (!isAllowedValue(value, allowedValues)) {
      return null;
    }
    props[name] = value;
  }
  return { element: declaredElement, event, props };
}

function requireAllowedValue(
  event: string,
  name: string,
  value: unknown,
  allowedValues: readonly string[],
): string {
  if (!isAllowedValue(value, allowedValues)) {
    throw new Error(
      `"${event}" needs "${name}" to be one of: ${allowedValues.join(", ")}. It got: ${String(value)}.`,
    );
  }
  return value;
}

function isAllowedValue(
  value: unknown,
  allowedValues: readonly string[],
): value is string {
  return typeof value === "string" && allowedValues.includes(value);
}

function isPlacement(value: string): value is Placement {
  return (PLACEMENTS as readonly string[]).includes(value);
}

function sendEvent(
  element: Element,
  eventName: string,
  props: Record<string, string>,
  options: TrackOptions = {},
): void {
  const propsWithPlacement: Record<string, string> = {
    ...props,
    placement: getPlacement(element),
  };

  // Plausible counts a visit as a bounce if it has one pageview and no
  // interactive event. A custom event is interactive by default, so a copy or
  // a click would turn a bounce into a non-bounce. `interactive: false` keeps
  // these events out of the Plausible bounce rate, so the bounce rate stays
  // comparable with the months before these events. Plausible still counts
  // the events themselves.
  window.plausible?.(eventName, {
    props: propsWithPlacement,
    interactive: false,
  });
  window.posthog?.capture?.(
    eventName,
    propsWithPlacement,
    options.leavesPage ? { transport: "sendBeacon" } : undefined,
  );
}
