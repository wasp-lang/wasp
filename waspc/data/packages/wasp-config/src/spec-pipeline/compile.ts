import { mkdirSync, readFileSync, writeFileSync } from "fs";
import { basename, dirname, join, resolve } from "path";
import * as ts from "typescript";
import { lowerSrcImports } from "./lowerSrcImports.js";

export type CompileWaspTsToJsInput = {
  inputPath: string;
  tsconfigPath: string;
  outputPath: string;
};

type VirtualSource = {
  path: string;
  text: string;
};

export function compileWaspTsToJs({
  inputPath,
  tsconfigPath,
  outputPath,
}: CompileWaspTsToJsInput): void {
  const tsConfig = readTsConfig(tsconfigPath);

  const rewrittenSource = makeRewrittenVirtualSource(inputPath, outputPath);
  const js = emitVirtualSourceToJs(rewrittenSource, tsConfig);

  writeJs(outputPath, js);
}

function readTsConfig(tsconfigPath: string): ts.ParsedCommandLine {
  const config = ts.readConfigFile(tsconfigPath, ts.sys.readFile);
  if (config.error) {
    throw new Error(formatDiagnostics([config.error]));
  }

  const parsedConfig = ts.parseJsonConfigFileContent(
    config.config,
    ts.sys,
    dirname(tsconfigPath),
    {},
    tsconfigPath,
  );

  throwIfDiagnostics(parsedConfig.errors);

  return parsedConfig;
}

function makeRewrittenVirtualSource(
  inputPath: string,
  outputPath: string,
): VirtualSource {
  const authoredSource = readFileSync(inputPath, "utf8");

  return {
    path: getVirtualSourcePath(inputPath, outputPath),
    text: lowerSrcImports(authoredSource),
  };
}

function emitVirtualSourceToJs(
  source: VirtualSource,
  tsConfig: ts.ParsedCommandLine,
): string {
  const options = getEmitOptions(tsConfig.options);
  const jsOutput = makeJsOutputCapture();
  const host = makeVirtualSourceHost({
    options,
    source,
    onJsOutput: jsOutput.set,
  });
  const program = ts.createProgram({
    rootNames: [source.path],
    options,
    host,
    projectReferences: tsConfig.projectReferences,
  });

  throwIfDiagnostics(ts.getPreEmitDiagnostics(program));

  const emitResult = program.emit();
  throwIfDiagnostics(emitResult.diagnostics);

  const js = jsOutput.get();
  if (!js || emitResult.emitSkipped) {
    throw new Error("TypeScript did not emit executable JavaScript.");
  }

  return js;
}

function getEmitOptions(options: ts.CompilerOptions): ts.CompilerOptions {
  return {
    ...options,
    declaration: false,
    declarationMap: false,
    emitDeclarationOnly: false,
    noEmit: false,
    outDir: undefined,
    outFile: undefined,
    sourceMap: false,
  };
}

function makeVirtualSourceHost({
  options,
  source,
  onJsOutput,
}: {
  options: ts.CompilerOptions;
  source: VirtualSource;
  onJsOutput: (js: string) => void;
}): ts.CompilerHost {
  const host = ts.createCompilerHost(options);
  const isVirtualSource = makePathPredicate(source.path);
  const getSourceFileFromDisk = host.getSourceFile.bind(host);
  const readFileFromDisk = host.readFile?.bind(host) ?? ts.sys.readFile;
  const fileExistsOnDisk = host.fileExists.bind(host);

  host.getSourceFile = (fileName, languageVersion, onError) => {
    if (isVirtualSource(fileName)) {
      return ts.createSourceFile(
        source.path,
        source.text,
        languageVersion,
        true,
        ts.ScriptKind.TS,
      );
    }

    return getSourceFileFromDisk(fileName, languageVersion, onError);
  };

  host.readFile = (fileName) =>
    isVirtualSource(fileName) ? source.text : readFileFromDisk(fileName);

  host.fileExists = (fileName) =>
    isVirtualSource(fileName) || fileExistsOnDisk(fileName);

  host.writeFile = (
    fileName,
    text,
    _writeByteOrderMark,
    _onError,
    sourceFiles,
  ) => {
    if (isEmittedJsFromVirtualSource(fileName, sourceFiles, isVirtualSource)) {
      onJsOutput(text);
    }
  };

  return host;
}

function makeJsOutputCapture(): {
  set: (js: string) => void;
  get: () => string | undefined;
} {
  let js: string | undefined;

  return {
    set: (nextJs) => {
      js = nextJs;
    },
    get: () => js,
  };
}

function isEmittedJsFromVirtualSource(
  fileName: string,
  sourceFiles: readonly ts.SourceFile[] | undefined,
  isVirtualSource: (fileName: string) => boolean,
): boolean {
  return (
    fileName.endsWith(".js") &&
    sourceFiles?.some((sourceFile) => isVirtualSource(sourceFile.fileName)) ===
      true
  );
}

function writeJs(outputPath: string, js: string): void {
  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, js, "utf8");
}

function getVirtualSourcePath(inputPath: string, outputPath: string): string {
  // Keep the old rewritten-file name in diagnostics without writing it to disk.
  return join(
    dirname(outputPath),
    basename(inputPath).replace(/\.ts$/, ".rewritten.ts"),
  );
}

function makePathPredicate(
  expectedPath: string,
): (fileName: string) => boolean {
  const expectedCanonicalPath = canonicalizePath(expectedPath);

  return (fileName) => canonicalizePath(fileName) === expectedCanonicalPath;
}

function canonicalizePath(fileName: string): string {
  const absolutePath = resolve(fileName);
  return ts.sys.useCaseSensitiveFileNames
    ? absolutePath
    : absolutePath.toLowerCase();
}

function throwIfDiagnostics(diagnostics: readonly ts.Diagnostic[]): void {
  if (diagnostics.length > 0) {
    throw new Error(formatDiagnostics(diagnostics));
  }
}

function formatDiagnostics(diagnostics: readonly ts.Diagnostic[]): string {
  return ts.formatDiagnostics(diagnostics, {
    getCanonicalFileName: (fileName) => fileName,
    getCurrentDirectory: ts.sys.getCurrentDirectory,
    getNewLine: () => ts.sys.newLine,
  });
}
