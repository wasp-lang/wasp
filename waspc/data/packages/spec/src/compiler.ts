import type { Plugin } from "rolldown";
import { transformWaspTsSpecFilesPlugin } from "./spec-pipeline/transformWaspTsSpecFilesPlugin/index.js";
import { typecheckPlugin } from "./spec-pipeline/typecheckPlugin/index.js";
import type { RefOrigin } from "./spec/refObject.js";

export type { RefOrigin };

export function createWaspTsSpecPlugins({
  getRefOrigin,
  tsconfigPath,
}: {
  getRefOrigin: (specFilePath: string) => RefOrigin;
  tsconfigPath: string;
}): Plugin[] {
  return [
    ...transformWaspTsSpecFilesPlugin({ getRefOrigin }),
    typecheckPlugin({ tsconfigPath }),
  ];
}
