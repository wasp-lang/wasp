import { mkdir, readFile, writeFile } from "fs/promises";
import { dirname } from "path";
import { lowerSrcImports } from "../lowerSrcImports.js";
import { compileTsSourceToJs } from "./inMemoryTsCompiler.js";

export async function compileWaspTsFileToJsFile({
  inputPath,
  tsconfigPath,
  outputPath,
}: {
  inputPath: string;
  tsconfigPath: string;
  outputPath: string;
}): Promise<void> {
  const [tsConfigSource, tsSpecSource] = await Promise.all([
    readFile(tsconfigPath, "utf8"),
    readFile(inputPath, "utf8"),
  ]);
  const js = compileTsSourceToJs({
    source: lowerSrcImports(tsSpecSource),
    sourcePath: inputPath,
    tsconfigPath,
    tsConfigSource,
  });

  await mkdir(dirname(outputPath), { recursive: true });
  await writeFile(outputPath, js, "utf8");
}
