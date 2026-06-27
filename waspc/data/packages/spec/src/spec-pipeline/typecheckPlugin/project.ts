import * as path from "node:path";
import ts from "typescript";

export function typecheckProject({
  tsconfigPath,
  overriddenFiles,
}: {
  tsconfigPath: string;
  overriddenFiles: ReadonlyMap<string, string>;
}) {
  const { options: compilerOptions, tsconfigDir } = parseTsConfig(tsconfigPath);

  const host = createCompilerHostWithOverriddenFiles({
    compilerOptions,
    tsconfigDir,
    overriddenFiles,
  });

  const program = ts.createProgram({
    rootNames: [...overriddenFiles.keys()],
    options: compilerOptions,
    host,
  });

  const diagnostics = ts.getPreEmitDiagnostics(program);

  const formatDiagnosticsWithColorAndContext = (
    diagnostics: readonly ts.Diagnostic[],
  ) => ts.formatDiagnosticsWithColorAndContext(diagnostics, host);

  return { diagnostics, formatDiagnosticsWithColorAndContext };
}

function parseTsConfig(tsconfigPath: string): {
  options: ts.CompilerOptions;
  tsconfigDir: string;
} {
  const tsconfigDir = path.dirname(tsconfigPath);

  const configFile = ts.readConfigFile(tsconfigPath, ts.sys.readFile);
  if (configFile.error) {
    throw new Error(
      `Error when reading ${tsconfigPath}:\n${formatConfigError(configFile.error, tsconfigDir)}`,
    );
  }

  // Converts the raw JSON (e.g. `"target": "ES2022"`) into the typed compiler
  // options the compiler API expects (e.g. `target: ts.ScriptTarget.ES2022`).
  const parsed = ts.parseJsonConfigFileContent(
    configFile.config,
    ts.sys,
    tsconfigDir,
    undefined,
    tsconfigPath,
  );
  if (parsed.errors.length > 0) {
    const formattedErrors = parsed.errors
      .map((error) => formatConfigError(error, tsconfigDir))
      .join("\n");
    throw new Error(`Error when parsing ${tsconfigPath}:\n${formattedErrors}`);
  }

  return { options: parsed.options, tsconfigDir };
}

function createCompilerHostWithOverriddenFiles({
  compilerOptions,
  tsconfigDir,
  overriddenFiles,
}: {
  compilerOptions: ts.CompilerOptions;
  tsconfigDir: string;
  overriddenFiles: ReadonlyMap<string, string>;
}): ts.CompilerHost {
  const host = ts.createCompilerHost(compilerOptions, true);

  // Resolve everything relative to the project root (where `node_modules` and
  // its `@types/*` packages live) so type acquisition works regardless of the
  // process' working directory.
  host.getCurrentDirectory = () => tsconfigDir;

  overlayOverriddenFilesOnHost_mutate(host, overriddenFiles);

  return host;
}

// We want to read the specific overridden files from memory instead of disk.
// For other files, we delegate to the normal FS implementation.
function overlayOverriddenFilesOnHost_mutate(
  host: ts.CompilerHost,
  overriddenFiles: ReadonlyMap<string, string>,
) {
  const normalizedOverriddenFiles = new Map<string, string>();
  for (const [key, value] of overriddenFiles.entries()) {
    normalizedOverriddenFiles.set(key.replace(/\\/g, "/"), value);
  }

  const fsGetSourceFile = host.getSourceFile.bind(host);
  host.getSourceFile = (fileName, languageVersionOrOptions, ...rest) => {
    const normalizedFileName = fileName.replace(/\\/g, "/");
    const source = normalizedOverriddenFiles.get(normalizedFileName);
    return source !== undefined
      ? ts.createSourceFile(fileName, source, languageVersionOrOptions, true)
      : fsGetSourceFile(fileName, languageVersionOrOptions, ...rest);
  };

  const fsFileExists = host.fileExists.bind(host);
  host.fileExists = (fileName) => {
    const normalizedFileName = fileName.replace(/\\/g, "/");
    return normalizedOverriddenFiles.has(normalizedFileName) || fsFileExists(fileName);
  };

  const fsReadFile = host.readFile.bind(host);
  host.readFile = (fileName) => {
    const normalizedFileName = fileName.replace(/\\/g, "/");
    return normalizedOverriddenFiles.get(normalizedFileName) ?? fsReadFile(fileName);
  };
}

function formatConfigError(diagnostic: ts.Diagnostic, cwd: string): string {
  return ts.formatDiagnostic(diagnostic, {
    getCurrentDirectory: () => cwd,
    getCanonicalFileName: (fileName) => fileName,
    getNewLine: () => ts.sys.newLine,
  });
}
