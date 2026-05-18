import { lowerSrcImports } from "./lowerSrcImports.js";

export type SpecSourceTransformer = {
  transformSource: (sourcePath: string, sourceText: string) => string;
};

export function createSpecSourceTransformer(): SpecSourceTransformer {
  return {
    transformSource: (sourcePath, sourceText) => {
      if (isWaspTsFile(sourcePath)) {
        return lowerSrcImports(sourceText, sourcePath);
      } else {
        // TODO: check if @src imports are used in files that are not
        // .wasp.ts files and throw an error if they are.
        return sourceText;
      }
    },
  };
}

function isWaspTsFile(sourcePath: string): boolean {
  return sourcePath.endsWith(".wasp.ts");
}
