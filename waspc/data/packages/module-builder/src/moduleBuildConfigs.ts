import {
  createWaspTsSpecPlugins,
  getRootRelativeSpecFilePath,
} from "@wasp.sh/spec/compiler";
import { readFileSync } from "node:fs";
import path from "node:path";
import type { UserConfig } from "tsdown";
import { cssPassthroughPlugin } from "./cssPassthroughPlugin.js";

type PackageJson = {
  name: string;
  wasp: {
    module: {
      runtimeExports?: string[];
    };
  };
};

export function getModuleBuildConfigs(moduleDir: string): UserConfig[] {
  const packageJson = readPackageJson(moduleDir);
  const runtimeExports =
    packageJson.wasp.module.runtimeExports ?? DEFAULT_RUNTIME_EXPORTS;
  const exportsConfig = {
    customExports(exports: Record<string, unknown>) {
      if (exports["."] !== undefined && exports["./spec"] === undefined) {
        exports["./spec"] = exports["."];
        delete exports["."];
      }
      return exports;
    },
  };

  const configs: UserConfig[] = [
    {
      name: "module-spec",
      entry: { spec: "./module.wasp.ts" },
      outDir: "dist",
      format: "esm",
      sourcemap: false,
      dts: { sourcemap: false },
      fixedExtension: false,
      clean: true,
      platform: "node",
      target: "node24",
      tsconfig: "tsconfig.wasp.json",
      exports: exportsConfig,
      plugins: createWaspTsSpecPlugins({
        tsconfigPath: path.join(moduleDir, "tsconfig.wasp.json"),
        getRefOrigin: (filePath) => ({
          kind: "package",
          packageName: packageJson.name,
          specFilePath: getRootRelativeSpecFilePath(moduleDir, filePath),
        }),
      }),
      deps: { neverBundle: [/^[^./]/] },
    },
  ];

  if (runtimeExports.length > 0) {
    configs.push({
      name: "module-runtime",
      entry: runtimeExports,
      outDir: "dist",
      format: "esm",
      sourcemap: true,
      dts: true,
      fixedExtension: false,
      clean: false,
      platform: "neutral",
      tsconfig: "tsconfig.src.json",
      exports: exportsConfig,
      plugins: [
        cssPassthroughPlugin({
          sourceDir: path.join(moduleDir, "src"),
          outDir: path.join(moduleDir, "dist"),
        }),
      ],
      deps: { neverBundle: ["react", "react/jsx-runtime", /^wasp\//] },
    });
  }

  return configs.map((config) => ({ ...config, cwd: moduleDir }));
}

export const DEFAULT_RUNTIME_EXPORTS = ["./src/**/*.{ts,tsx}"];

function readPackageJson(moduleDir: string): PackageJson {
  return JSON.parse(
    readFileSync(path.join(moduleDir, "package.json"), "utf8"),
  ) as PackageJson;
}
