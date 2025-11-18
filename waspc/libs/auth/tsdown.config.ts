import { defineConfig, type UserConfig } from "tsdown";

const commonTsDownOptions: UserConfig = {
  outDir: "dist",
  dts: {
    sourcemap: true,
  },
  sourcemap: true,
};

function createNewEntry({
  name,
  entryPath,
  platform,
}: {
  name: string;
  entryPath: string;
  platform: UserConfig["platform"];
}): UserConfig {
  return {
    ...commonTsDownOptions,
    entry: {
      [name]: entryPath,
    },
    platform,
    // When using `node` platform, this option ensures the output extension remains `.js` and not `.mjs`.
    fixedExtension: false,
  };
}

export default defineConfig([
  createNewEntry({
    name: "sdk",
    entryPath: "./src/sdk/index.ts",
    platform: "neutral",
  }),
  createNewEntry({
    name: "sdk-browser",
    entryPath: "./src/sdk/browser/index.ts",
    platform: "browser",
  }),
  createNewEntry({
    name: "server",
    entryPath: "./src/server/index.ts",
    platform: "node",
  }),
]);
