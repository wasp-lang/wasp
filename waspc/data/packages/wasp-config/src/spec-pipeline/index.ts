import { readFile } from "fs/promises";
import type { TransformOptions, TransformResult } from "jiti";
import { createJiti } from "jiti";
import { pathToFileURL } from "url";
import {
  createSpecSourceTransformer,
  type SpecSourceTransformer,
} from "./specSourceTransform.js";

export async function loadWaspTsFileDefaultExport({
  inputPath,
  tsconfigPath,
}: {
  inputPath: string;
  tsconfigPath: string;
}): Promise<unknown> {
  const tsSpecSource = await readFile(inputPath, "utf8");

  const loadedModule = await loadTsSource({
    source: tsSpecSource,
    sourcePath: inputPath,
    tsconfigPath,
    sourceTransformer: createSpecSourceTransformer(),
  });

  return getDefaultExport(loadedModule);
}

async function loadTsSource({
  source,
  sourcePath,
  tsconfigPath,
  sourceTransformer,
}: {
  source: string;
  sourcePath: string;
  tsconfigPath: string;
  sourceTransformer: SpecSourceTransformer;
}): Promise<unknown> {
  const defaultJiti = createJiti(pathToFileURL(sourcePath).href, {
    fsCache: false,
    interopDefault: false,
    jsx: true,
    moduleCache: false,
    tsconfigPaths: tsconfigPath,
  });

  const jiti = createJiti(pathToFileURL(sourcePath).href, {
    fsCache: false,
    interopDefault: false,
    jsx: true,
    moduleCache: false,
    tsconfigPaths: tsconfigPath,
    transform: (options) =>
      transformWithLoweredSpecSources(options, sourceTransformer, defaultJiti),
  });

  return jiti.evalModule(source, {
    filename: sourcePath,
    ext: ".ts",
    async: true,
    forceTranspile: true,
  });
}

function transformWithLoweredSpecSources(
  options: TransformOptions,
  sourceTransformer: SpecSourceTransformer,
  defaultJiti: ReturnType<typeof createJiti>,
): TransformResult {
  const transformedSource = options.filename
    ? sourceTransformer.transformSource(options.filename, options.source)
    : options.source;
  const code = defaultJiti.transform({ ...options, source: transformedSource });

  return { code };
}

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
