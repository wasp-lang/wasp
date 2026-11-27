// Scope logic shared by the server (token checks, step-up challenges) and the
// browser (the consent screen's checkboxes), so both agree on what a set of
// scopes means.

export type ScopeDefinition = {
  /** Shown next to the checkbox on the consent screen. */
  label: string;
  description?: string;
  /** Scopes that come with this one, e.g. write implies read. */
  implies?: readonly string[];
};

export type ScopeDefinitions = Readonly<Record<string, ScopeDefinition>>;

/** Keeps only declared scopes, in declaration order, without duplicates. */
export function normalizeScopes(
  definitions: ScopeDefinitions,
  scopes: Iterable<string>,
): string[] {
  const wanted = new Set(scopes);
  return Object.keys(definitions).filter((scope) => wanted.has(scope));
}

/** Adds every scope the given ones imply, transitively. */
export function expandScopes(
  definitions: ScopeDefinitions,
  scopes: Iterable<string>,
): string[] {
  const result = new Set<string>();
  const visit = (scope: string) => {
    if (result.has(scope) || !(scope in definitions)) return;
    result.add(scope);
    for (const implied of definitions[scope]?.implies ?? []) visit(implied);
  };
  for (const scope of scopes) visit(scope);
  return normalizeScopes(definitions, result);
}

/** The user's choice, limited to what the client asked for. */
export function capScopes(
  definitions: ScopeDefinitions,
  chosen: Iterable<string>,
  ceiling: Iterable<string>,
): string[] {
  const allowed = new Set(expandScopes(definitions, ceiling));
  return expandScopes(definitions, chosen).filter((scope) =>
    allowed.has(scope),
  );
}

/**
 * Checkbox logic for scope pickers: ticking a scope ticks what it implies,
 * and unticking a scope unticks every scope that implies it.
 */
export function toggleScope(
  definitions: ScopeDefinitions,
  selected: readonly string[],
  scope: string,
  checked: boolean,
): string[] {
  if (checked) return expandScopes(definitions, [...selected, scope]);
  return normalizeScopes(
    definitions,
    selected.filter(
      (s) => s !== scope && !expandScopes(definitions, [s]).includes(scope),
    ),
  );
}
