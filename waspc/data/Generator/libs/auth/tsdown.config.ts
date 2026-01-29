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
    name: "index",
    entryPath: "./src/index.ts",
    platform: "neutral",
  }),
  createNewEntry({
    name: "browser",
    entryPath: "./src/browser/index.ts",
    platform: "browser",
  }),
  createNewEntry({
    name: "node",
    entryPath: "./src/node/index.ts",
    platform: "node",
  }),
]);
