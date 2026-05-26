
export function caseByKind<T extends { kind: PropertyKey }, R> (
  value: T,
  cases: { [K in T["kind"]]: (value: Extract<T, { kind: K }>) => R }
) {
  return cases[value.kind as T["kind"]](value as Extract<T, { kind: T["kind"] }>);
}
