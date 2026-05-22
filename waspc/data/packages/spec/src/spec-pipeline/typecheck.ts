import * as path from "node:path";
import * as ts from "typescript";
import { SpecUserError } from "../spec/specUserError.js";

export async function typecheck<T>(
  { tsconfigPath }: { tsconfigPath: string },
  fn: (ctx: {
    addSourceFile: (path: string, code: string) => void;
  }) => Promise<T>,
): Promise<T> {
  const { config, error: readError } = ts.readConfigFile(
    tsconfigPath,
    ts.sys.readFile,
  );
  if (readError) {
    throw new Error(
      ts.flattenDiagnosticMessageText(readError.messageText, "\n"),
    );
  }

  const parsedConfig = ts.parseJsonConfigFileContent(
    config,
    ts.sys,
    path.dirname(tsconfigPath),
  );

  const compilerOptions = parsedConfig.options;

  const inMemoryFiles = new Map<string, string>();
  const loadedWaspTsFiles = new Set<string>();

  const host = ts.createCompilerHost(compilerOptions, true);
  const originalReadFile = host.readFile.bind(host);
  const originalFileExists = host.fileExists.bind(host);

  host.readFile = (fileName) => {
    const inMemory = inMemoryFiles.get(normalize(fileName));
    return inMemory ?? originalReadFile(fileName);
  };
  host.fileExists = (fileName) => {
    if (inMemoryFiles.has(normalize(fileName))) return true;
    return originalFileExists(fileName);
  };

  const result = await promiseToResult(
    fn({
      addSourceFile: (filePath, code) => {
        const normalized = normalize(filePath);
        inMemoryFiles.set(normalized, code);
        if (isWaspTsPath(normalized)) {
          loadedWaspTsFiles.add(normalized);
        }
      },
    }),
  );

  if (
    result.type === "error" &&
    result.error instanceof Error &&
    result.error.message.startsWith("ParseError")
  ) {
    throw result.error;
  }

  // Combine the in-memory files with every other file matched by the tsconfig.
  // Ref imports have been lowered so the typechecker won't follow them.
  const rootFiles = new Set<string>(inMemoryFiles.keys());
  for (const filePath of parsedConfig.fileNames) {
    const normalized = normalize(filePath);
    // Only `.wasp.ts` files reachable from the loaded spec have had their ref
    // imports lowered; other `.wasp.ts` files matched by the tsconfig include
    // patterns still contain unlowered ref imports and would produce spurious
    // type errors, so drop them before resolving dependencies.
    if (isWaspTsPath(normalized) && !loadedWaspTsFiles.has(normalized)) {
      continue;
    }
    rootFiles.add(normalized);
  }

  const program = ts.createProgram({
    rootNames: Array.from(rootFiles),
    options: compilerOptions,
    host,
  });

  const diagnostics = ts.getPreEmitDiagnostics(program);

  if (diagnostics.length > 0) {
    const formatHost: ts.FormatDiagnosticsHost = {
      getCanonicalFileName: (f) => host.getCanonicalFileName(f),
      getCurrentDirectory: () => host.getCurrentDirectory(),
      getNewLine: () => host.getNewLine(),
    };
    console.error(
      ts.formatDiagnosticsWithColorAndContext(diagnostics, formatHost),
    );
  }

  if (diagnostics.some((d) => d.category === ts.DiagnosticCategory.Error)) {
    throw new SpecUserError("Type errors found");
  }

  if (result.type === "error") {
    throw result.error;
  } else {
    return result.value;
  }
}

function isWaspTsPath(path: string): boolean {
  return path.endsWith(".wasp.ts");
}

function normalize(filePath: string): string {
  return path.resolve(filePath).split(path.sep).join("/");
}

function promiseToResult<T>(
  promise: Promise<T>,
): Promise<{ type: "success"; value: T } | { type: "error"; error: unknown }> {
  return promise.then(
    (value) => ({ type: "success", value }),
    (error) => ({ type: "error", error }),
  );
}
