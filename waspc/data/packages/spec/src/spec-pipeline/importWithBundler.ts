import type { JitiOptions } from "jiti";
import { createJiti } from "jiti";
import { transformWaspTsSpecSource } from "./transform/index.js";

export async function importWithBundler({
  specPath,
  tsconfigPath,
  onTransformedWaspFile,
}: {
  specPath: string;
  tsconfigPath: string;
  onTransformedWaspFile: (path: string, code: string) => void;
}): Promise<unknown> {
  const specJiti = createSpecJiti({
    specPath,
    tsconfigPath,
    onTransformedWaspFile,
  });

  return await specJiti.import(specPath);
}

function createSpecJiti({
  specPath,
  tsconfigPath,
  onTransformedWaspFile,
}: {
  specPath: string;
  tsconfigPath: string;
  onTransformedWaspFile: (path: string, code: string) => void;
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
  // have transformed the Wasp TS source.
  const jitiWithoutCustomTransform = createJiti(specPath, jitiOptions);

  return createJiti(specPath, {
    ...jitiOptions,
    transform: (options) => {
      let transformedSource = options.source;

      if (options.filename && isWaspTsFile(options.filename)) {
        transformedSource = transformWaspTsSpecSource({
          sourceText: options.source,
          sourcePath: options.filename,
        });
        onTransformedWaspFile(options.filename, transformedSource);
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
