import type { ViteUserConfig } from "vitest/config";

export type TestConfig = NonNullable<ViteUserConfig["test"]>;

const defaultTestConfig = {
  globals: true,
  environment: "jsdom",
  include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
  env: {
    // Tests run without `wasp start`, which provides the server URL in
    // development. They mock the server, so a placeholder is enough.
    REACT_APP_API_URL: "http://localhost:3001",
  },
} satisfies TestConfig;

/**
 * Returns Vitest's `test` options with the defaults Wasp apps need: global
 * test APIs, a `jsdom` environment, test files in `src`, and a placeholder
 * server URL. Your options override the defaults, and your `env` variables are
 * added to Wasp's.
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
