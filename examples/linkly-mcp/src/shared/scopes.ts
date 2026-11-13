// Single source of truth for permissions: app.mcp.scopes in main.wasp.ts
// declares these for the consent screen, and the services check them.
type ScopeDefinitions = Record<
  string,
  { label: string; implies?: readonly string[] }
>;

export const SCOPES = {
  "links:read": { label: "View your links" },
  "links:write": {
    label: "Create, edit, and delete your links",
    implies: ["links:read"],
  },
  "analytics:read": { label: "View click analytics" },
} as const satisfies ScopeDefinitions;
export type Scope = keyof typeof SCOPES;
export const ALL_SCOPES = Object.keys(SCOPES) as Scope[];

export function isScope(value: string): value is Scope {
  return value in SCOPES;
}
