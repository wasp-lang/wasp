import { mkdirSync, readFileSync, writeFileSync } from "fs";
import { basename, dirname, join, resolve } from "path";
import * as ts from "typescript";
import { rewrite } from "./rewrite.js";

export function compileWaspTsToJs({
  inputPath,
  tsconfigPath,
  outputPath,
}: {
  inputPath: string;
  tsconfigPath: string;
  outputPath: string;
}): void {
  const parsedConfig = readTsConfig(tsconfigPath);
  typeCheckAuthoredSource(inputPath, parsedConfig);

  const sourceText = readFileSync(inputPath, "utf8");
  const rewrittenSource = rewrite(sourceText);
  const js = emitVirtualWaspTsToJs({
    rewrittenSource,
    virtualSourcePath: getVirtualSourcePath(inputPath, outputPath),
    parsedConfig,
  });

  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, js, "utf8");
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
  if (parsedConfig.errors.length > 0) {
    throw new Error(formatDiagnostics(parsedConfig.errors));
  }

  return parsedConfig;
}

function typeCheckAuthoredSource(
  inputPath: string,
  parsedConfig: ts.ParsedCommandLine,
): void {
  const options: ts.CompilerOptions = {
    ...parsedConfig.options,
    noEmit: true,
  };
  const program = ts.createProgram({
    rootNames: [inputPath],
    options,
    projectReferences: parsedConfig.projectReferences,
  });
  const diagnostics = ts.getPreEmitDiagnostics(program);

  if (diagnostics.length > 0) {
    throw new Error(formatDiagnostics(diagnostics));
  }
}

function emitVirtualWaspTsToJs({
  rewrittenSource,
  virtualSourcePath,
  parsedConfig,
}: {
  rewrittenSource: string;
  virtualSourcePath: string;
  parsedConfig: ts.ParsedCommandLine;
}): string {
  const options: ts.CompilerOptions = {
    ...parsedConfig.options,
    declaration: false,
    declarationMap: false,
    emitDeclarationOnly: false,
    noEmit: false,
    outDir: undefined,
    outFile: undefined,
    sourceMap: false,
  };
  const { host, getEmittedJs } = makeVirtualCompilerHost({
    options,
    rewrittenSource,
    virtualSourcePath,
  });
  const program = ts.createProgram({
    rootNames: [virtualSourcePath],
    options,
    host,
    projectReferences: parsedConfig.projectReferences,
  });

  const preEmitDiagnostics = ts.getPreEmitDiagnostics(program);
  if (preEmitDiagnostics.length > 0) {
    throw new Error(formatDiagnostics(preEmitDiagnostics));
  }

  const emitResult = program.emit();
  if (emitResult.diagnostics.length > 0) {
    throw new Error(formatDiagnostics(emitResult.diagnostics));
  }

  const emittedJs = getEmittedJs();
  if (!emittedJs || emitResult.emitSkipped) {
    throw new Error("TypeScript did not emit executable JavaScript.");
  }

  return emittedJs;
}

function makeVirtualCompilerHost({
  options,
  rewrittenSource,
  virtualSourcePath,
}: {
  options: ts.CompilerOptions;
  rewrittenSource: string;
  virtualSourcePath: string;
}): { host: ts.CompilerHost; getEmittedJs: () => string | undefined } {
  const host = ts.createCompilerHost(options);
  const canonicalVirtualSourcePath = canonicalizePath(virtualSourcePath);
  const originalGetSourceFile = host.getSourceFile.bind(host);
  const originalReadFile = host.readFile?.bind(host) ?? ts.sys.readFile;
  const originalFileExists = host.fileExists.bind(host);
  let emittedJs: string | undefined;

  host.getSourceFile = (fileName, languageVersion, onError) => {
    if (isVirtualSourcePath(fileName, canonicalVirtualSourcePath)) {
      return ts.createSourceFile(
        virtualSourcePath,
        rewrittenSource,
        languageVersion,
        true,
        ts.ScriptKind.TS,
      );
    }

    return originalGetSourceFile(fileName, languageVersion, onError);
  };

  host.readFile = (fileName) =>
    isVirtualSourcePath(fileName, canonicalVirtualSourcePath)
      ? rewrittenSource
      : originalReadFile(fileName);

  host.fileExists = (fileName) =>
    isVirtualSourcePath(fileName, canonicalVirtualSourcePath)
      ? true
      : originalFileExists(fileName);

  host.writeFile = (
    fileName,
    text,
    _writeByteOrderMark,
    _onError,
    sourceFiles,
  ) => {
    if (
      fileName.endsWith(".js") &&
      sourceFiles?.some((sourceFile) =>
        isVirtualSourcePath(sourceFile.fileName, canonicalVirtualSourcePath),
      )
    ) {
      emittedJs = text;
    }
  };

  return { host, getEmittedJs: () => emittedJs };
}

function getVirtualSourcePath(inputPath: string, outputPath: string): string {
  return join(
    dirname(outputPath),
    basename(inputPath).replace(/\.ts$/, ".rewritten.ts"),
  );
}

function isVirtualSourcePath(
  fileName: string,
  canonicalVirtualSourcePath: string,
): boolean {
  return canonicalizePath(fileName) === canonicalVirtualSourcePath;
}

function canonicalizePath(fileName: string): string {
  const absolutePath = resolve(fileName);
  return ts.sys.useCaseSensitiveFileNames
    ? absolutePath
    : absolutePath.toLowerCase();
}

function formatDiagnostics(diagnostics: readonly ts.Diagnostic[]): string {
  return ts.formatDiagnostics(diagnostics, {
    getCanonicalFileName: (fileName) => fileName,
    getCurrentDirectory: ts.sys.getCurrentDirectory,
    getNewLine: () => ts.sys.newLine,
  });
}
