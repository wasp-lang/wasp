import { defineBuildConfig } from "unbuild";

export default defineBuildConfig({
  entries: ["./src/index.ts"],
  outDir: "./dist",
  declaration: true,
  clean: true,
  sourcemap: false,
});
