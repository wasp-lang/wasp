import type { JitiOptions } from "jiti";
import { createJiti } from "jiti";
import { lowerRefImports } from "./lowerRefImports.js";

export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
  projectRootDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
}): Promise<unknown> {
  const specJiti = createSpecJiti({ specPath, tsconfigPath, projectRootDir });
  const specModule = await specJiti.import(specPath);

  return getDefaultExport(specModule);
}

function createSpecJiti({
  specPath,
  tsconfigPath,
  projectRootDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
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
      const transformedSource =
        options.filename && isWaspTsFile(options.filename)
          ? lowerRefImports({
              sourceText: options.source,
              sourcePath: options.filename,
              projectRootDir,
            })
          : options.source;

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

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
