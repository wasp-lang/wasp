import type { ViteUserConfig } from "vitest/config";

export type TestConfig = NonNullable<ViteUserConfig["test"]>;

const defaultTestConfig = {
  environment: "jsdom",
  include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
  env: {
    // Tests mock the server, so they get a URL that never resolves (`.invalid`
    // is a reserved domain). A request the test didn't mock fails instead of
    // reaching a server that happens to run on the machine.
    REACT_APP_API_URL: "http://fake-testing-server.invalid",
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
