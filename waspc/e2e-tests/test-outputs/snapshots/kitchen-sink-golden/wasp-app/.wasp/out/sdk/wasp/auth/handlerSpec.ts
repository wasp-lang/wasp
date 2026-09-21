// PRIVATE API
/**
 * Rebuilds an auth handler's `spec` for its factory.
 *
 * A handler's spec is one object mixing plain data with references to app
 * code. A reference cannot cross the compiler as data, so the compiler lifted
 * each one out, keyed by the path it sat at. This is the inverse: the data,
 * with every reference set back at its path, so the factory receives the
 * object the spec helper built -- with live functions where the references
 * were.
 *
 * A path is an array of object keys; the compiler rejects references inside
 * arrays, so every segment is a key of a plain object.
 */
export function joinHandlerSpec(
  data: unknown,
  references: ReadonlyArray<readonly [path: readonly string[], value: unknown]>,
): unknown {
  if (references.length === 0) {
    return data
  }
  const spec: Record<string, unknown> = isRecord(data) ? structuredClone(data) : {}
  for (const [path, value] of references) {
    let parent = spec
    for (const key of path.slice(0, -1)) {
      // The compiler dropped the reference's own key, so a parent that held
      // nothing else is missing from the data entirely.
      if (!isRecord(parent[key])) {
        parent[key] = {}
      }
      parent = parent[key] as Record<string, unknown>
    }
    parent[path[path.length - 1]!] = value
  }
  return spec
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
}
