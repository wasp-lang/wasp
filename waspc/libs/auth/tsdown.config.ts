import { defineConfig, type Options } from "tsdown";

const commonTsDownOptions: Options = {
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
  platform: Options["platform"];
}): Options {
  return {
    ...commonTsDownOptions,
    entry: {
      [name]: entryPath,
    },
    platform,
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
