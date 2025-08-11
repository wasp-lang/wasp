import { defineConfig, type Options } from "tsdown";

const commonTsDownOptions: Options = {
  outDir: "dist",
  dts: true,
  sourcemap: true,
};

export default defineConfig([
  {
    ...commonTsDownOptions,
    entry: {
      index: "./src/index.ts",
    },
    platform: "neutral",
  },
  {
    ...commonTsDownOptions,
    entry: {
      client: "./src/client/index.ts",
    },
    platform: "browser",
  },
]);
