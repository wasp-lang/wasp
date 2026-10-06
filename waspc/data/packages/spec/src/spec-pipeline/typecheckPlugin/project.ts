import * as fs from "node:fs";
import * as path from "node:path";
import {
  API,
  DiagnosticCategory,
  type Diagnostic,
} from "typescript/unstable/sync";
import { createAnalyzerFileSystem } from "./analyzerFileSystem.js";
import {
  formatDiagnostics,
  formatDiagnosticsWithColorAndContext,
  sortAndDeduplicateDiagnostics,
  type DiagnosticsFormatHost,
} from "./formatDiagnostics.js";

// Lives next to the user's tsconfig (so `${configDir}` in it keeps its
// meaning), but only in the analyzer's virtual file system.
const ANALYZER_TSCONFIG_FILE_NAME = "tsconfig.wasp-analyzer.json";

export function typecheckProject({
  tsconfigPath,
  projectRootDir,
  overriddenFiles,
}: {
  tsconfigPath: string;
  projectRootDir: string;
  overriddenFiles: ReadonlyMap<string, string>;
}): {
  diagnostics: readonly Diagnostic[];
  formatDiagnosticsWithColorAndContext: (
    diagnostics: readonly Diagnostic[],
  ) => string;
} {
  const tsconfigDir = path.dirname(tsconfigPath);
  const analyzerTsconfigPath = path.join(
    tsconfigDir,
    ANALYZER_TSCONFIG_FILE_NAME,
  );

  // The user's compiler options, applied only to the spec files.
  const analyzerTsconfig = {
    extends: tsconfigPath,
    files: [...overriddenFiles.keys()],
    include: [],
  };

  const fileSystem = createAnalyzerFileSystem({
    projectRootDir,
    virtualFiles: new Map([
      ...overriddenFiles,
      [analyzerTsconfigPath, JSON.stringify(analyzerTsconfig)],
    ]),
  });

  const formatHost: DiagnosticsFormatHost = {
    currentDirectory: tsconfigDir,
    readFile: (fileName) =>
      fileSystem.readFile(fileName) ?? readFileIfExists(fileName),
  };

  const api = new API({ cwd: tsconfigDir, fs: fileSystem });
  try {
    const snapshot = api.updateSnapshot({
      openProjects: [analyzerTsconfigPath],
    });
    const project = snapshot.getProject(analyzerTsconfigPath);
    if (!project) {
      throw new Error(`TypeScript didn't load ${tsconfigPath}.`);
    }
    const { program } = project;

    const configDiagnostics = program.getConfigFileParsingDiagnostics();
    if (configDiagnostics.some(isError)) {
      throw new Error(
        `Error when parsing ${tsconfigPath}:\n${formatDiagnostics(configDiagnostics, formatHost)}`,
      );
    }

    const diagnostics = sortAndDeduplicateDiagnostics([
      ...configDiagnostics,
      ...program.getProgramDiagnostics(),
      ...program.getSyntacticDiagnostics(),
      ...program.getGlobalDiagnostics(),
      ...program.getSemanticDiagnostics(),
      ...(project.compilerOptions.declaration ||
      project.compilerOptions.composite
        ? program.getDeclarationDiagnostics()
        : []),
    ]);

    return {
      diagnostics,
      formatDiagnosticsWithColorAndContext: (diagnostics) =>
        formatDiagnosticsWithColorAndContext(diagnostics, formatHost),
    };
  } finally {
    api.close();
  }
}

function isError(diagnostic: Diagnostic): boolean {
  return diagnostic.category === DiagnosticCategory.Error;
}

function readFileIfExists(fileName: string): string | undefined {
  try {
    return fs.readFileSync(fileName, "utf8");
  } catch {
    return undefined;
  }
}
