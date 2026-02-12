import * as React from 'react';

// PUBLIC API
/**
 * Interface for a user-provided SSR styles provider.
 *
 * Wasp calls `createSsrStylesProvider()` once per SSR request.  The returned
 * object tells Wasp how to wrap the React tree and how to extract the
 * generated CSS after rendering.
 *
 * ## Usage
 *
 * Create `src/ssr/styles.tsx` and export a `createSsrStylesProvider` function:
 *
 * ```tsx
 * // src/ssr/styles.tsx — Emotion example
 * import { CacheProvider } from '@emotion/react';
 * import createCache from '@emotion/cache';
 * import createEmotionServer from '@emotion/server/create-instance';
 * import type { SsrStylesProvider } from 'wasp/client/ssr';
 *
 * export function createSsrStylesProvider(): SsrStylesProvider {
 *   const cache = createCache({ key: 'css' });
 *   const { extractCriticalToChunks, constructStyleTagsFromChunks } =
 *     createEmotionServer(cache);
 *
 *   return {
 *     Wrapper: ({ children }) => (
 *       <CacheProvider value={cache}>{children}</CacheProvider>
 *     ),
 *     extractStyles: (appHtml) => {
 *       const chunks = extractCriticalToChunks(appHtml);
 *       return constructStyleTagsFromChunks(chunks);
 *     },
 *   };
 * }
 * ```
 */
export interface SsrStylesProvider {
  /**
   * Optional React component that wraps the application tree during SSR.
   *
   * Use this to provide context (e.g. Emotion's `CacheProvider`,
   * styled-components' `StyleSheetManager`, etc.) that your CSS-in-JS
   * library needs during server-side rendering.
   */
  Wrapper?: React.ComponentType<{ children: React.ReactNode }>;

  /**
   * Called after `renderToString` to extract CSS style tags that should be
   * injected into the HTML `<head>`.
   *
   * @param appHtml - The HTML string produced by `renderToString`.
   * @returns A string of `<style>` tags (or empty string if none).
   */
  extractStyles?: (appHtml: string) => string;
}

// PUBLIC API
/**
 * Factory function type that users export from `src/ssr/styles.tsx`.
 *
 * Wasp calls this once per SSR request to get a fresh provider instance.
 * A new instance per request ensures no style leakage between requests.
 */
export type CreateSsrStylesProvider = () => SsrStylesProvider;
