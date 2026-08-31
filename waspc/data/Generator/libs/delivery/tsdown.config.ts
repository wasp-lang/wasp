import { defineConfig, type UserConfig } from "tsdown";

const commonTsDownOptions: UserConfig = {
  outDir: "dist",
  dts: {
    sourcemap: true,
  },
  sourcemap: true,
};

function createEntry({
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
    fixedExtension: false,
  };
}

export default defineConfig([
  createEntry({
    name: "index",
    entryPath: "./src/index.ts",
    platform: "neutral",
  }),
  createEntry({
    name: "browser",
    entryPath: "./src/browser.ts",
    platform: "browser",
  }),
  createEntry({ name: "node", entryPath: "./src/node.ts", platform: "node" }),
]);
