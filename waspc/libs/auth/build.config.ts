import { defineBuildConfig } from "unbuild";

export default defineBuildConfig({
  entries: ["./src/index.ts", "./src/client/index.ts"],
  outDir: "./dist",
  declaration: true,
  clean: true,
  sourcemap: false,
});
