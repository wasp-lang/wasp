import { rmSync } from "node:fs";
import esbuild from "rollup-plugin-esbuild";
import resolve from "@rollup/plugin-node-resolve";

export default {
  input: {
    bootstrap: "src/bootstrap.ts",
  },
  output: {
    dir: "build",
    entryFileNames: "[name].js",
    format: "es",
    sourcemap: true,
  },
  plugins: [
    cleanOutputDir(),
    resolve({ extensions: [".mjs", ".js", ".ts", ".json", ".node"] }),
    esbuild({
      target: "esnext",
    }),
  ],
  external: (id) =>
    id === "wasp" ||
    id.startsWith("wasp/") ||
    id.includes("node_modules"),
  preserveSymlinks: true,
};

function cleanOutputDir() {
  return {
    name: "clean-output-dir",
    buildStart() {
      rmSync("build", { force: true, recursive: true });
    },
  };
}
