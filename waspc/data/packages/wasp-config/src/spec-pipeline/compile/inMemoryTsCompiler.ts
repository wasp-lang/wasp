import { dirname, resolve } from "path";
import * as ts from "typescript";

export function compileTsSourceToJs({
  source,
  sourcePath,
  tsconfigPath,
  tsConfigSource,
}: {
  source: string;
  sourcePath: string;
  tsconfigPath: string;
  tsConfigSource: string;
}): string {
  const tsConfig = parseTsConfigOrThrow(tsconfigPath, tsConfigSource);

  return compileTsSourceWithConfigToJs(
    {
      fileName: sourcePath,
      text: source,
    },
    tsConfig,
  );
}

function parseTsConfigOrThrow(
  tsconfigPath: string,
  tsConfigSource: string,
): ts.ParsedCommandLine {
  const config = ts.parseConfigFileTextToJson(tsconfigPath, tsConfigSource);
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

function compileTsSourceWithConfigToJs(
  source: InMemoryTsSource,
  tsConfig: ts.ParsedCommandLine,
): string {
  const options: ts.CompilerOptions = {
    ...tsConfig.options,
    declaration: false,
    declarationMap: false,
    emitDeclarationOnly: false,
    noEmit: false,
    sourceMap: false,
    inlineSourceMap: false,
  };

  return emitInMemoryTsSourceToJs({
    options,
    projectReferences: tsConfig.projectReferences,
    source,
  });
}

type InMemoryTsSource = {
  fileName: string;
  text: string;
};

/**
 * Compiles a single in-memory file while resolving all other files from disk.
 */
function emitInMemoryTsSourceToJs({
  options,
  projectReferences,
  source,
}: {
  options: ts.CompilerOptions;
  projectReferences?: readonly ts.ProjectReference[];
  source: InMemoryTsSource;
}): string {
  const emittedJsTexts: string[] = [];

  const host = createCompilerHostForInMemorySource({
    options,
    source,
    onJsEmit: (text) => emittedJsTexts.push(text),
  });
  const program = ts.createProgram({
    rootNames: [source.fileName],
    options,
    host,
    projectReferences,
  });
  throwIfDiagnostics(ts.getPreEmitDiagnostics(program));

  const emitResult = program.emit();
  throwIfDiagnostics(emitResult.diagnostics);

  return getOnlyEmittedJsText(emitResult, emittedJsTexts);
}

/**
 * Creates a TypeScript compiler host that reads the input source from memory,
 * reads dependencies from disk, and reports emitted JS through a callback.
 */
function createCompilerHostForInMemorySource({
  options,
  source,
  onJsEmit,
}: {
  options: ts.CompilerOptions;
  source: InMemoryTsSource;
  onJsEmit: (text: string) => void;
}): ts.CompilerHost {
  const host = ts.createCompilerHost(options);

  const isInputSourceFile = (() => {
    const canonicalize = (fileName: string) =>
      host.getCanonicalFileName(resolve(fileName));
    const sourceFileName = canonicalize(source.fileName);

    return (fileName: string) => canonicalize(fileName) === sourceFileName;
  })();

  const getSourceFileFromDisk = host.getSourceFile.bind(host);
  const readFileFromDisk = host.readFile?.bind(host) ?? ts.sys.readFile;
  const fileExistsOnDisk = host.fileExists.bind(host);

  host.getSourceFile = (fileName, languageVersion, onError) => {
    if (isInputSourceFile(fileName)) {
      return ts.createSourceFile(
        source.fileName,
        source.text,
        languageVersion,
        true,
        ts.ScriptKind.TS,
      );
    }

    return getSourceFileFromDisk(fileName, languageVersion, onError);
  };

  host.readFile = (fileName) =>
    isInputSourceFile(fileName) ? source.text : readFileFromDisk(fileName);

  host.fileExists = (fileName) =>
    isInputSourceFile(fileName) || fileExistsOnDisk(fileName);

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
        isInputSourceFile(sourceFile.fileName),
      ) === true
    ) {
      onJsEmit(text);
    }
  };

  return host;
}

function getOnlyEmittedJsText(
  emitResult: ts.EmitResult,
  emittedJsTexts: string[],
): string {
  const emittedJsText = emittedJsTexts.at(0);

  if (emitResult.emitSkipped || !emittedJsText) {
    throw new Error("TypeScript did not emit executable JavaScript.");
  }

  if (emittedJsTexts.length > 1) {
    throw new Error(
      "TypeScript emitted multiple JavaScript files for the Wasp TS spec.",
    );
  }

  return emittedJsText;
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
