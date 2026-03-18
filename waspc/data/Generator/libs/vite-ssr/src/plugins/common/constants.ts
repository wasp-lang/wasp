export const PACKAGE_NAME = "@wasp.sh/lib-vite-ssr" as const;

// This environment names come from Vite's own built-in environments, see
// https://vite.dev/guide/api-environment#closing-the-gap-between-build-and-dev.
export const ENVIRONMENT_NAMES = {
  SSR: "ssr",
  CLIENT: "client",
} as const;
