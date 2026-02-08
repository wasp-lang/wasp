/**
 * SSR Styles Provider -- Emotion / MUI
 *
 * This file is auto-detected by Wasp's SSR pipeline. It must export a
 * `createSsrStylesProvider` function that returns `{ Wrapper, extractStyles }`.
 *
 * Wasp calls this once per SSR request to:
 *   1. Wrap the React tree with Emotion's CacheProvider (so MUI styled()
 *      components have a valid cache during renderToString).
 *   2. Extract the generated CSS after rendering and inject it into <head>,
 *      eliminating FOUC and ensuring hydration sees matching styles.
 *
 * @see https://emotion.sh/docs/ssr -- "Advanced Approach"
 */
import React from 'react';
import createCache from '@emotion/cache';
import { CacheProvider } from '@emotion/react';
import createEmotionServer from '@emotion/server/create-instance';

export function createSsrStylesProvider() {
  const cache = createCache({ key: 'css' });

  // createEmotionServer sets cache.compat = true, which makes the internal
  // _insert store actual CSS text (not just `true`) in cache.inserted and
  // suppresses inline <style> tags during renderToString. This enables the
  // Advanced Approach where extractCriticalToChunks collects all CSS for the
  // <head> -- no nth-child selector issues.
  const { extractCriticalToChunks, constructStyleTagsFromChunks } =
    createEmotionServer(cache);

  return {
    Wrapper: ({ children }: { children: React.ReactNode }) => (
      <CacheProvider value={cache}>{children}</CacheProvider>
    ),

    extractStyles: (appHtml: string): string => {
      const chunks = extractCriticalToChunks(appHtml);
      return constructStyleTagsFromChunks(chunks);
    },
  };
}
