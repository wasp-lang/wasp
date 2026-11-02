import type { ViteUserConfig } from "vitest/config";

export type TestConfig = NonNullable<ViteUserConfig["test"]>;

const defaultTestConfig = {
  environment: "jsdom",
  include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
  env: {
    // Tests run without `wasp start`, which provides the server URL in
    // development. They mock the server, so a placeholder is enough.
    REACT_APP_API_URL: "http://localhost:3001",
  },
} satisfies TestConfig;

/**
 * Returns Vitest's `test` options with the defaults Wasp apps need:
 * - `environment`: `jsdom`
 * - `include`: test files in `src`
 * - `env`: a placeholder server URL
 *
 * Your options override the defaults, only the `env` is merged instead.
 *
 * @see https://vitest.dev/config/
 */
export function waspTestConfig(userConfig: TestConfig = {}): TestConfig {
  return {
    ...defaultTestConfig,
    ...userConfig,
    env: { ...defaultTestConfig.env, ...userConfig.env },
  };
}
