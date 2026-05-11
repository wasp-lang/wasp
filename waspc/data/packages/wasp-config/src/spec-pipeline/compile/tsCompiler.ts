import { dirname, resolve } from "path";
import * as ts from "typescript";

type TsSource = {
  fileName: string;
  text: string;
};

type EmittedJsFile = {
  fileName: string;
  text: string;
};

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

  return compileTsSource(
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

function compileTsSource(
  source: TsSource,
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
  const emittedJsFiles: EmittedJsFile[] = [];
  const host = makeCompilerHostWithSource({
    options,
    source,
    onJsOutput: (output) => emittedJsFiles.push(output),
  });
  const program = ts.createProgram({
    rootNames: [source.fileName],
    options,
    host,
    projectReferences: tsConfig.projectReferences,
  });

  throwIfDiagnostics(ts.getPreEmitDiagnostics(program));

  const emitResult = program.emit();
  throwIfDiagnostics(emitResult.diagnostics);

  return getOnlyEmittedJsFile(emitResult, emittedJsFiles).text;
}

function getOnlyEmittedJsFile(
  emitResult: ts.EmitResult,
  emittedJsFiles: EmittedJsFile[],
): EmittedJsFile {
  const [emittedJsFile, extraJsFile] = emittedJsFiles;

  if (emitResult.emitSkipped || !emittedJsFile) {
    throw new Error("TypeScript did not emit executable JavaScript.");
  }

  if (extraJsFile) {
    throw new Error(
      `TypeScript emitted multiple JavaScript files for the Wasp TS spec: ${emittedJsFiles
        .map((file) => file.fileName)
        .join(", ")}.`,
    );
  }

  return emittedJsFile;
}

function makeCompilerHostWithSource({
  options,
  source,
  onJsOutput,
}: {
  options: ts.CompilerOptions;
  source: TsSource;
  onJsOutput: (output: EmittedJsFile) => void;
}): ts.CompilerHost {
  const host = ts.createCompilerHost(options);
  const sourceFileName = canonicalizePath(source.fileName);
  const isSource = (fileName: string) =>
    canonicalizePath(fileName) === sourceFileName;
  const getSourceFileFromDisk = host.getSourceFile.bind(host);
  const readFileFromDisk = host.readFile?.bind(host) ?? ts.sys.readFile;
  const fileExistsOnDisk = host.fileExists.bind(host);

  host.getSourceFile = (fileName, languageVersion, onError) => {
    if (isSource(fileName)) {
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
    isSource(fileName) ? source.text : readFileFromDisk(fileName);

  host.fileExists = (fileName) => isSource(fileName) || fileExistsOnDisk(fileName);

  host.writeFile = (
    fileName,
    text,
    _writeByteOrderMark,
    _onError,
    sourceFiles,
  ) => {
    if (
      fileName.endsWith(".js") &&
      sourceFiles?.some((sourceFile) => isSource(sourceFile.fileName)) === true
    ) {
      onJsOutput({ fileName, text });
    }
  };

  return host;
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
