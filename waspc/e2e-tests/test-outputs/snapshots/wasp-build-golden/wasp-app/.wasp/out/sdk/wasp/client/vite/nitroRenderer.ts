import { fetchViteEnv } from "nitro/vite/runtime";

/**
 * Nitro's renderer: the catch-all handler that runs for requests that didn't
 * match a static asset or a route.
 *
 * It only forwards to the `ssr` Vite environment, where `ssr-entry.tsx` does
 * the actual rendering. The split is not optional: Nitro bundles this module
 * twice, once with Vite and once with a bare Rollup/Rolldown build (for
 * prerendering) that has none of Vite's plugins. So this file must stay plain
 * TypeScript: no JSX, no CSS imports, no `?assets` imports, no Vite-only
 * features.
 */
export default function renderer({ req }: { req: Request }): Promise<Response> {
  return fetchViteEnv("ssr", req);
}
