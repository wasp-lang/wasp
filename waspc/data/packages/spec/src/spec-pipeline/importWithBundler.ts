import type { JitiOptions } from "jiti";
import { createJiti } from "jiti";
import { lowerRefImports } from "./lowerRefImports.js";

export async function importWithBundler({
  specPath,
  tsconfigPath,
  projectRootDir,
  overwriteTSFile,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
  overwriteTSFile: (path: string, code: string) => void;
}): Promise<unknown> {
  const specJiti = createSpecJiti({
    specPath,
    tsconfigPath,
    projectRootDir,
    overwriteTSFile,
  });

  return await specJiti.import(specPath);
}

function createSpecJiti({
  specPath,
  tsconfigPath,
  projectRootDir,
  overwriteTSFile,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
  overwriteTSFile: (path: string, code: string) => void;
}) {
  const jitiOptions = {
    fsCache: false,
    interopDefault: false,
    jsx: true,
    moduleCache: false,
    tsconfigPaths: tsconfigPath,
  } satisfies JitiOptions;

  // Using a custom `transform` function replaces the default jiti
  // transform, so we need another jiti instance to call it after we
  // have lowered the imports in the source.
  const jitiWithoutCustomTransform = createJiti(specPath, jitiOptions);

  return createJiti(specPath, {
    ...jitiOptions,
    transform: (options) => {
      let transformedSource = options.source;

      if (options.filename && isWaspTsFile(options.filename)) {
        transformedSource = lowerRefImports({
          sourceText: options.source,
          sourcePath: options.filename,
          projectRootDir,
        });
        overwriteTSFile(options.filename, transformedSource);
      }

      const code = jitiWithoutCustomTransform.transform({
        ...options,
        source: transformedSource,
      });

      return { code };
    },
  });
}

function isWaspTsFile(sourcePath: string): boolean {
  return sourcePath.endsWith(".wasp.ts");
}
