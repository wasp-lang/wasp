import esbuild from "rollup-plugin-esbuild";
import resolve from "@rollup/plugin-node-resolve";
import { virtualUserModules } from "./src/plugins/virtualUserModules.js";

const authProviderPackages = ['@wasp.sh/auth'];

export default [
  createBundle("src/server.ts", "bundle/server.js"),
  createBundle("src/dbSeed.ts", "bundle/dbSeed.js"),
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
    // We don't want to bundle any of the node_module deps because we want to
    // keep them as external dependencies. Auth provider packages are external
    // by name too: a package installed through a `file:` link resolves to a
    // realpath outside node_modules, and inlining it would strip it of its own
    // dependencies.
    external: (id) =>
      /node_modules/.test(id) ||
      authProviderPackages.some((pkg) => id === pkg || id.startsWith(pkg + "/")),
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
