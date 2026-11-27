/**
 * The helper for configuring [Vitest](https://vitest.dev/) to test your
 * client code. The helpers you use inside your tests are in
 * `@wasp/test-utils`.
 *
 * @module
 */

import type { ViteUserConfig } from "vitest/config";
import { apiUrlEnvVarName } from "./apiUrl.js";

/**
 * The `test` options of your Vitest config, which {@link waspTestConfig} takes
 * and returns.
 */
export type TestConfig = NonNullable<ViteUserConfig["test"]>;

const defaultTestConfig = {
  environment: "jsdom",
  include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
  env: {
    // Tests mock the server, so they get a URL that never resolves (`.invalid`
    // is a reserved domain). A request the test didn't mock fails instead of
    // reaching a server that happens to run on the machine.
    [apiUrlEnvVarName]: "http://fake-testing-server.invalid",
  },
} satisfies TestConfig;

/**
 * Returns the Vitest options for the `test` section of your `vite.config.ts`,
 * with the defaults a Wasp app needs:
 *
 * - `environment`: `"jsdom"`, a simulated browser, so your components can
 *   render.
 * - `include`: the test files in your `src` folder (e.g. `TaskList.test.tsx`
 *   or `helpers.spec.js`).
 * - `env`: a placeholder `REACT_APP_API_URL`, since your tests mock the
 *   server instead of running it.
 *
 * @param userConfig [Vitest options](https://vitest.dev/config/). They
 * override the defaults. Only `env` is merged with the default `env`, and your
 * variables take precedence.
 *
 * @example
 * ```ts
 * import { waspTestConfig } from "@wasp/test-utils/config";
 * import { defineConfig } from "vitest/config";
 * import { wasp } from "wasp/client/vite";
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
export function waspTestConfig(userConfig: TestConfig = {}): TestConfig {
  return {
    ...defaultTestConfig,
    ...userConfig,
    env: { ...defaultTestConfig.env, ...userConfig.env },
  };
}
