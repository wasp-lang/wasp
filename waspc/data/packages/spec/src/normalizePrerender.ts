/**
 * Normalizes the public `prerender` route option into the AppSpec
 * representation: a list of concrete paths to prerender at build time, or
 * `undefined` when prerendering is disabled.
 *
 * - `true` is shorthand for "prerender this route's own path".
 * - `false`/`undefined`/`[]` disable prerendering.
 * - A non-empty list of paths is used as-is.
 */
export function normalizePrerender(
  prerender: boolean | readonly string[] | undefined,
  routePath: string,
): string[] | undefined {
  if (prerender === undefined || prerender === false) {
    return undefined;
  }
  if (prerender === true) {
    return [routePath];
  }
  return prerender.length > 0 ? [...prerender] : undefined;
}
