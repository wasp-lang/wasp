import * as path from "node:path";
import type { Plugin } from "vite";
import {
  getImportedPackageName,
  isWaspDependencyImport,
} from "../../sdk/index.js";

type NoExternal = true | string | RegExp | readonly (string | RegExp)[];

/**
 * Keeps the packages that come with `@wasp/wasp` out of the bundle that
 * prerenders the app, so Node imports them when the bundle runs.
 *
 * Vite does that on its own for the packages that the project can find, but
 * only some package managers let a project find a package it doesn't list.
 * The others would end up in the bundle, where a package like
 * `@prisma/client` breaks, because it looks for files next to itself.
 */
export function externalWaspDependencies(generatedAppDir: string): Plugin {
  return {
    name: "wasp:external-wasp-dependencies",
    enforce: "pre",
    apply: "build",
    applyToEnvironment: (environment) =>
      environment.config.consumer === "server",
    resolveId(source) {
      const isExternal =
        isWaspDependencyImport(generatedAppDir, source) &&
        isJsImport(source) &&
        !isConfiguredAsNoExternal(
          this.environment.config.resolve.noExternal,
          source,
        );
      return isExternal ? { id: source, external: true } : null;
    },
  };
}

/** Node can't import the other files a package can have, like its CSS. */
function isJsImport(importSpecifier: string): boolean {
  const packageName = getImportedPackageName(importSpecifier) ?? "";
  const pathInPackage = importSpecifier.slice(packageName.length);
  return ["", ".js", ".mjs", ".cjs"].includes(path.extname(pathInPackage));
}

function isConfiguredAsNoExternal(
  noExternal: NoExternal,
  importSpecifier: string,
): boolean {
  if (noExternal === true) {
    return true;
  }
  const packageName = getImportedPackageName(importSpecifier);
  return [noExternal]
    .flat()
    .some((pattern) =>
      typeof pattern === "string"
        ? pattern === packageName
        : packageName !== undefined && pattern.test(packageName),
    );
}
