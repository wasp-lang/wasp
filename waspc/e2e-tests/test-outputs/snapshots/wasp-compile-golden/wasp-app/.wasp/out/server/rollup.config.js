import esbuild from "rollup-plugin-esbuild";
import resolve from "@rollup/plugin-node-resolve";
import { resolve as resolvePath } from "node:path";
import {
  isVirtualUserModuleId,
  virtualUserModules,
} from "./src/plugins/virtualUserModules.js";
import {
  discoverWaspModulePackages,
  shouldExternalize as shouldExternalizePackageImport,
} from "./rollupPackages.js";

const appRootDir = resolvePath(import.meta.dirname, "../../..");
const packagesToBundle = new Set([
  "wasp",
  ...discoverWaspModulePackages(appRootDir),
]);

export default [
  createBundle("src/server.ts", "bundle/server.js"),
];

function createBundle(inputFilePath, outputFilePath) {
  return {
    input: inputFilePath,
    output: {
      file: outputFilePath,
      format: "es",
      sourcemap: true,
    },
    plugins: [
      virtualUserModules(),
      // We added `".ts"` to the default `extensions` array value
      // (default is `[".mjs", ".js", ".json", ".node"]`).
      // This is because the `virtualUserModules` plugin
      // can resolve user virtual modules to TypeScript files.
      resolve({ extensions: [".mjs", ".js", ".ts", ".json", ".node"] }),
      esbuild({
        target: "esnext",
      }),
    ],
    // Wasp modules must share the host's bundled Wasp runtime. Their own
    // third-party dependencies remain external.
    external: shouldExternalize,
    // 'preserveSymlinks: false' tells Rollup to fully follow symlinks when
    // resolving modules. This is the default option, but we're setting it
    // explicitly because we rely on it.
    //
    // With this set to 'false', the 'wasp' package resolves to
    // '.wasp/out/sdk/wasp' (not 'node_modules/wasp'), preventing Rollup from
    // viewing it as an external dependency and ensuring it gets bundled.
    //
    // We need to bundle 'wasp' to support fully extensionless relative
    // imports. See https://github.com/wasp-lang/wasp/issues/2492 for more
    // details.
    //
    // Source: https://rollupjs.org/configuration-options/#preservesymlinks
    preserveSymlinks: false,
  }
}

function shouldExternalize(moduleId) {
  if (isVirtualUserModuleId(moduleId)) {
    return false;
  }

  return shouldExternalizePackageImport(moduleId, packagesToBundle);
}
