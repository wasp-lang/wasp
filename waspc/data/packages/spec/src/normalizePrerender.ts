/**
 * Normalizes the public `prerender` route option into the AppSpec
 * representation: the list of concrete paths to prerender at build time (empty
 * when prerendering is disabled).
 *
 * - `true` is shorthand for "prerender this route's own path".
 * - `false`/`undefined`/`[]` disable prerendering (empty list).
 * - A list of paths is used as-is.
 */
export function normalizePrerender(
  prerender: boolean | readonly string[] | undefined,
  routePath: string,
): string[] {
  if (!prerender) {
    return [];
  }
  if (prerender === true) {
    return [routePath];
  }
  return [...prerender];
}
