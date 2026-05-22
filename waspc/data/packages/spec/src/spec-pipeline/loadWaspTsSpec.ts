import type { JitiOptions } from "jiti";
import { createJiti } from "jiti";
import { lowerRefImports } from "./lowerRefImports.js";
import { typecheck } from "./typecheck.js";

export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
  projectRootDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
}): Promise<unknown> {
  const specModule = await typecheck(
    { tsconfigPath },
    async ({ addSourceFile }) => {
      const specJiti = createSpecJiti({
        specPath,
        tsconfigPath,
        projectRootDir,
        overwriteTSFile: addSourceFile,
      });
      return await specJiti.import(specPath);
    },
  );

  return getDefaultExport(specModule);
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

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
