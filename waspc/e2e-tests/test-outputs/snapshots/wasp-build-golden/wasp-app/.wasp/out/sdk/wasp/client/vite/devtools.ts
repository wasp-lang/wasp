import { waspLogo } from "@wasp/lib-devtools/node";
import type { UserConfig } from "vite";

type DevToolsOptions = Exclude<UserConfig["devtools"], boolean | undefined>;

/**
 * Options for Vite's `devtools` config that brand Vite DevTools as Wasp
 * DevTools. Wasp Studio and the other panels work without it.
 *
 * @example
 * ```ts
 * export default defineConfig({
 *   devtools: waspDevTools(),
 *   plugins: [wasp()],
 * });
 * ```
 *
 * @param options Other Vite DevTools options, merged over Wasp's.
 */
export function waspDevTools(options: DevToolsOptions = {}): DevToolsOptions {
  return {
    apply: "serve",
    enabled: !process.env["VITEST"],
    ...options,
    builtinDevTools: withoutViteIntegrations(options.builtinDevTools),
    branding: {
      productName: "Wasp",
      windowTitle: "Wasp DevTools",
      tagline: "DevTools for your Wasp app",
      primaryColor: "#bf9900",
      logo: waspLogo,
      // Vite expects a wordmark image here, and will fallback to the logo +
      // text label if empty.
      wordmark: "",
      favicon: waspLogo,
      ...options.branding,
    },
  };
}

type BuiltinDevToolsOptions = DevToolsOptions["builtinDevTools"];

/**
 * Hides the Vite+ integrations. They depend on specific devtool libraries that
 * Wasp doesn't install, so the panels are no-ops anyway.
 */
function withoutViteIntegrations(
  options: BuiltinDevToolsOptions = {},
): BuiltinDevToolsOptions {
  if (typeof options === "boolean") {
    return options;
  }
  const { integrations = {} } = options;
  return {
    ...options,
    integrations:
      typeof integrations === "boolean"
        ? integrations
        : {
            rolldown: false,
            vite: false,
            vitest: false,
            oxc: false,
            ...integrations,
          },
  };
}
