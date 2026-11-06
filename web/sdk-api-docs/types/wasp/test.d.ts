/**
 * The helper for configuring [Vitest](https://vitest.dev/) to test your
 * client code. The helpers you use inside your tests are in
 * `wasp/client/test`.
 *
 * @module
 */

import type { TestConfig } from "@wasp/lib-test-utils/config";

/**
 * Returns Vitest's `test` options with the defaults a Wasp app needs:
 *
 * - `environment`: `"jsdom"`, a simulated browser, so your components can
 *   render.
 * - `include`: the test files in your `src` folder (e.g., `TaskList.test.tsx`
 *   or `helpers.spec.js`).
 * - `env`: a placeholder `REACT_APP_API_URL`, since your tests mock the
 *   server instead of running it.
 *
 * @param options [Vitest options](https://vitest.dev/config/). They override
 * the defaults, only the `env` is merged instead.
 * @returns The options for the `test` section of your `vite.config.ts`.
 *
 * @example
 * ```ts
 * import { defineConfig } from "vitest/config";
 * import { wasp } from "wasp/client/vite";
 * import { waspTestConfig } from "wasp/test";
 *
 * export default defineConfig({
 *   plugins: [wasp()],
 *   test: waspTestConfig({
 *     setupFiles: ["./src/test/setup.ts"],
 *   }),
 * });
 * ```
 *
 * @see [Testing](https://wasp.sh/docs/advanced/testing)
 */
export declare function waspTestConfig(options?: TestConfig): TestConfig;

/**
 * The `test` options of your Vitest config, which {@link waspTestConfig} takes
 * and returns.
 */
export type { TestConfig };
