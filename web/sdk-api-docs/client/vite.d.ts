/**
 * The Vite plugin that powers your app's client. Use it in your
 * `vite.config.ts`.
 *
 * @module
 */

import type { WaspPluginOptions as LibWaspPluginOptions } from "@wasp.sh/lib-sdk-core/node/vite";

/**
 * A Vite plugin, or a (possibly nested, possibly async) array of them.
 *
 * This is a simplified version of Vite's
 * [`PluginOption`](https://vite.dev/guide/api-plugin) type, which is what you
 * pass in the `plugins` array of your Vite config.
 */
type PluginOption = VitePluginOptionValue | Promise<VitePluginOptionValue>;

/**
 * A Vite plugin object (simplified to its required `name`), a falsy value
 * (which Vite ignores), or an array of {@link PluginOption}s.
 */
type VitePluginOptionValue =
  | { name: string }
  | false
  | null
  | undefined
  | PluginOption[];

/** The options of the {@link wasp} Vite plugin. */
export interface WaspPluginOptions {
  /**
   * Options for the underlying
   * [`@vitejs/plugin-react`](https://github.com/vitejs/vite-plugin-react/tree/main/packages/plugin-react)
   * plugin.
   *
   * This allows you to configure React-specific options like Babel plugins,
   * Fast Refresh settings, and JSX configuration.
   */
  reactOptions?: LibWaspPluginOptions["reactOptions"];
}

/**
 * The Vite plugin that provides all the essential Wasp features to your
 * client:
 *
 * - The configuration that Wasp full-stack apps need to work.
 * - Env vars validation.
 * - Prevention of server imports in client code.
 * - TypeScript type checking during production builds.
 *
 * You must use it in your `vite.config.ts`, and it must be the **first**
 * plugin in the `plugins` array. Add any other plugins (like Tailwind CSS)
 * after it.
 *
 * The plugin merges the rest of your Vite config with Wasp's defaults, but it
 * enforces some values that Wasp needs, and throws an error if you set them
 * yourself. See the
 * [enforced options](https://wasp.sh/docs/advanced/client-customization/custom-vite-config#enforced-options).
 *
 * @param options Options to customize the plugin.
 * @returns The Wasp plugins, to put in the `plugins` array of your Vite
 * config.
 *
 * @example
 * ```ts
 * // vite.config.ts
 * import { wasp } from "wasp/client/vite";
 * import { defineConfig } from "vite";
 *
 * export default defineConfig({
 *   plugins: [
 *     wasp({
 *       reactOptions: {
 *         // Pass any @vitejs/plugin-react options here
 *       },
 *     }),
 *   ],
 * });
 * ```
 *
 * @see [Custom Vite config](https://wasp.sh/docs/advanced/client-customization/custom-vite-config)
 */
export declare function wasp(options?: WaspPluginOptions): PluginOption;

// Only the declarations marked with `export` are exported.
export {};
