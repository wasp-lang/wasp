import type { JitiOptions } from "jiti";
import { createJiti } from "jiti";
import { dirname, join } from "node:path";
import { lowerSrcImports } from "./lowerSrcImports.js";

export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
}: {
  specPath: string;
  tsconfigPath: string;
}): Promise<unknown> {
  const specJiti = createSpecJiti(specPath, tsconfigPath);
  const specModule = await specJiti.import(specPath);

  return getDefaultExport(specModule);
}

function createSpecJiti(entryPath: string, tsconfigPath: string) {
  const jitiOptions = {
    fsCache: false,
    interopDefault: false,
    jsx: true,
    moduleCache: false,
    alias: {
      "@src/": join(dirname(tsconfigPath), "src/"),
    },
    tsconfigPaths: tsconfigPath,
  } satisfies JitiOptions;

  // Using a custom `transform` function replaces the default jiti
  // transform, so we need another jiti instance to call it after we
  // have lowered the imports in the source.
  const jitiWithoutCustomTransform = createJiti(entryPath, jitiOptions);

  return createJiti(entryPath, {
    ...jitiOptions,
    transform: (options) => {
      const transformedSource =
        options.filename && isWaspTsFile(options.filename)
          ? lowerSrcImports({
              sourceText: options.source,
              sourcePath: options.filename,
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
