import { DiagnosticCategory, Project } from "ts-morph";
import { SpecUserError } from "../spec/specUserError.js";

export async function typecheck<T>(
  {
    tsconfigPath,
  }: {
    /**
     * @remarks
     * This should only be `null` when used in tests, where there isn't an
     * actual tsconfig project on disk.
     */
    tsconfigPath: string | null;
  },

  fn: (ctx: {
    addSourceFile: (path: string, code: string) => void;
  }) => Promise<T>,
): Promise<T> {
  const project = new Project({
    tsConfigFilePath: tsconfigPath ?? undefined,
    skipAddingFilesFromTsConfig: true,
    defaultCompilerOptions: {
      // For some reason `ts-morph` doesn't pick up `@types/*` packages if we
      // don't explicitly set this. (important for `@types/node` globals)
      types: ["*"],
    },
  });

  const result = await promiseToResult(
    fn({
      addSourceFile: (path, code) => {
        project.createSourceFile(path, code, { overwrite: true });
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

  project.resolveSourceFileDependencies();

  const diagnostics = project.getPreEmitDiagnostics();

  if (diagnostics.length > 0) {
    console.error(project.formatDiagnosticsWithColorAndContext(diagnostics));
  }

  if (diagnostics.some((d) => d.getCategory() === DiagnosticCategory.Error)) {
    throw new SpecUserError("Type errors found");
  }

  if (result.type === "error") {
    throw result.error;
  } else {
    return result.value;
  }
}

function promiseToResult<T>(
  promise: Promise<T>,
): Promise<{ type: "success"; value: T } | { type: "error"; error: unknown }> {
  return promise.then(
    (value) => ({ type: "success", value }),
    (error) => ({ type: "error", error }),
  );
}
