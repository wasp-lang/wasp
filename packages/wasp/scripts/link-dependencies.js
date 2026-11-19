import {
  getGeneratedAppDir,
  linkWaspDependencies,
} from "@wasp/sdk-core/node/sdk";
import * as path from "node:path";

// Run from the project dir. Node gives the real path of this file, which is
// where the package manager keeps this package next to its dependencies.
const projectDir = process.cwd();
linkWaspDependencies({
  waspPackageDir: path.resolve(import.meta.dirname, ".."),
  projectDir,
  generatedAppDir: getGeneratedAppDir(projectDir),
});
