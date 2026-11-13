import type { UserConfig } from "vite";

type DevToolsOptions = Exclude<UserConfig["devtools"], boolean | undefined>;

// The Wasp logo (`web/static/img/wasp-logo.svg` in the Wasp repo).
const waspLogo = "data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjE1MjAiIHZpZXdCb3g9IjAgMCAxNTIwIDE1MjAiIHdpZHRoPSIxNTIwIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciPjxwYXRoIGQ9Im0wIDBoMTUyMHYxNTIwaC0xNTIweiIgZmlsbD0iI2Y1Yzg0MiIvPjxnIGZpbGw9IiMxMTEiPjxwYXRoIGQ9Im0xMDA3IDMwMC0xNTYuOTguMDAyLjQ5OSAxNDIuNTFoODUuODU3Yy4wMjM5LjAwMiAxOC43NjMgMS41NjYgMzAuNDYxIDcuOTEwOCAxMy41MzcgNy4zNDI1IDE0LjYyNiAxMC43NjMgMjEuNzA0IDIwLjQxNyAzLjA2NzQgNC4xODM1IDQuOTY1MSAxMC42ODcgNi4wNDk0IDE2LjEyMyAxLjA4MjcgNS40Mjg0IDEuMzc1NCA5Ljc2NDIgMS4zNzYyIDkuNzc2MnYxNDQuMzNsLjAwMi4wMjUycy41ODMgNy4xMDkzIDEuNzk2IDE1LjkxOGMxLjIxMyA4LjgwOSAzLjA0NDggMTkuMzA1IDUuNjA3OSAyNi4yMDUgNC4zNjc2IDExLjc1OSAxMy42MiAyNi42NTkgMjEuNjMzIDM2LjM2MyA5LjM4NjYgMTEuMzY3IDIyLjIyOCAyMC4xMjQgMzQuOTY5IDI3LjU2OCAxMS4yOTQgNi41OTg0IDMyLjYyOCAxMS41NDIgMzMuNTQ4IDExLjc1NHYyLjE4NDdjLS45MjAyLjIxMjMzLTIyLjI1NCA1LjE1NTUtMzMuNTQ4IDExLjc1NC0xMi43NDEgNy40NDM5LTI1LjU4MyAxNi4yMDEtMzQuOTcgMjcuNTY4LTguMDEzNCA5LjcwNDEtMTcuMjY2IDI0LjYwNC0yMS42MzMgMzYuMzYzLTIuNTYzMSA2LjkwMDctNC4zOTQ4IDE3LjM5Ni01LjYwNzggMjYuMjA1cy0xLjc5NiAxNS45MTgtMS43OTYgMTUuOTE4bC0uMDAyLjAyNzF2MTQ0LjMzYy0uMDAwODIuMDEyLS4yOTM0OCA0LjM0NzgtMS4zNzYyIDkuNzc2Mi0xLjA4NDIgNS40MzU5LTIuOTgyIDExLjk0LTYuMDQ5NCAxNi4xMjMtNy4wNzg5IDkuNjU0Ny04LjE2NzEgMTMuMDc1LTIxLjcwNCAyMC40MTgtMTEuNjk4IDYuMzQ0OC0zMC40MzcgNy45MTA3LTMwLjQ2MSA3LjkxMDdoLTg1Ljg1N2wtLjQ5OSAxNDIuNTFoMTU2Ljk5czcuNTQ2MS0uMTg3MiAxNi40ODMtLjYyOWM4LjkzNzEtLjQ0MTggMTkuMjM4LTEuMTI5NiAyNC44ODktMi4xNjUzIDcuMTgwOS0xLjMxNjUgMjAuMTAyLTMuMTYxNiAzMy41My05LjA3MTkgMTMuMzI5LTUuODY2OCAxNi4yOTktNy4zNjQ3IDIyLjQ0My0xMC45MzkgMTEuNzU4LTYuODQwOCAyNC4xNjktMTkuMTQ2IDM0LjQ0MS0zMi4zMTFzMTguMzg5LTI3LjE2NyAyMS40NjMtMzcuNTI0YzYuMTIzNS0yMC42MzIgNi4zODItNDcuMTI2IDYuMzgyLTQ3LjEyNnYtMTk1LjM2YzAtLjAyMDcuMTQ1LTIuNTgwMS41MDUtNS43Mzk2LjM2MTQtMy4xNzIyLjk1Ni02Ljk0MTYgMS43NzQyLTkuMTUxMi45NTA0LTIuNTY2NCAyLjg5MTEtNy4yOTk5IDUuMzUwNC0xMS45MDUgMi40NTkyLTQuNjA0OSA1LjQ4MS05LjA4NDEgOC4zNzAxLTExLjExNSA1LjY0My0zLjk2NzQgMTIuODA5LTkuNTgyMiAyMC4yOS0xMC42MSA1LjkxMzMtLjgxMjUyIDEzLjE4Mi0xLjQzOTcgMTMuMjAyLTEuNDQxN2g1My44OTJ2LTE0OS44M2gtNTMuODkyYy0uMDE5LS4wMDItNy4yODg0LS42MjkxNS0xMy4yMDItMS40NDE3LTcuNDgxNC0xLjAyOC0xNC42NDgtNi42NDI5LTIwLjI5LTEwLjYxLTIuODg5MS0yLjAzMTItNS45MTA5LTYuNTEwNC04LjM3MDEtMTEuMTE1LTIuNDU5My00LjYwNDktNC40LTkuMzM4NC01LjM1MDQtMTEuOTA1LS44MTgyLTIuMjA5NS0xLjQxMjgtNS45Nzg5LTEuNzc0Mi05LjE1MTItLjM2LTMuMTU5NS0uNTAzOC01LjcxODgtLjUwNS01LjczOTZ2LTE5NS4zNnMtLjI1ODUtMjYuNDk0LTYuMzgyLTQ3LjEyNmMtMy4wNzM5LTEwLjM1Ny0xMS4xOTEtMjQuMzU5LTIxLjQ2My0zNy41MjRzLTIyLjY4My0yNS40Ny0zNC40NDEtMzIuMzExYy02LjE0MzgtMy41NzQ1LTkuMTE0MS01LjA3MjMtMjIuNDQzLTEwLjkzOS0xMy40MjctNS45MTAyLTI2LjM0OS03Ljc1NTQtMzMuNTMtOS4wNzE4LTUuNjUwMy0xLjAzNTgtMTUuOTUyLTEuNzIzNi0yNC44ODktMi4xNjU0LTguOTM2Ni0uNDQxOC0xNi40ODMtLjYyODkyLTE2LjQ4My0uNjI4OTJ6Ii8+PHBhdGggZD0ibTI1MCA4MzBoNTAwdjE2MGgtNTAweiIgc3Ryb2tlLXdpZHRoPSIxLjM0NzIiLz48cGF0aCBkPSJtMjUwIDUzMGg1MDB2MTYwaC01MDB6IiBzdHJva2Utd2lkdGg9IjEuMzQ3MiIvPjwvZz48L3N2Zz4=";

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
    // Only on the dev server, not in `vite build`. Vitest starts a Vite server
    // too, but there is no page to show DevTools on.
    apply: "serve",
    enabled: !process.env["VITEST"],
    ...options,
    branding: {
      productName: "Wasp DevTools",
      windowTitle: "Wasp DevTools",
      tagline: "DevTools for your Wasp app",
      primaryColor: "#bf9900",
      logo: waspLogo,
      // Empty, so the header shows the logo next to the product name instead
      // of Vite's wordmark.
      wordmark: "",
      favicon: waspLogo,
      ...options.branding,
    },
  };
}
