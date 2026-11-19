import resolve from "@rollup/plugin-node-resolve";
import esbuild from "rollup-plugin-esbuild";
import { sdkImports } from "./plugins/sdkImports.js";
import { virtualUserModules } from "./plugins/virtualUserModules.js";

/**
 * @param {string} inputFilePath
 * @param {string} outputFilePath
 * @returns {import("rollup").RollupOptions}
 */
export function createRollupBundle(inputFilePath, outputFilePath) {
  return {
    input: inputFilePath,
    output: {
      file: outputFilePath,
      format: "es",
      sourcemap: true,
    },
    plugins: [
      sdkImports(),
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
    // keep them as external dependencies.
    external: /node_modules/,
  }
}
