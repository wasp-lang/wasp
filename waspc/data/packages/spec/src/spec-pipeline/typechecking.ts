import { DiagnosticCategory, Project } from "ts-morph";
import { SpecUserError } from "../spec/specUserError.js";

/*
  We do some wrangling because we want to do things in a specific order, to
  match expectations of users:

  1. Read source files
  2. Lower imports
  3. Typecheck
  4. Compile to JS
  5. Run

  However, the typescript and jiti APIs don't directly expose the primitives to
  be able to do this. TypeScript will, by default, read and typecheck everything
  before transforming, and jiti will transform and execute on the fly in one go.

  Therefore, we will let jiti transform and run the code, overwriting lowered
  imports as we finde them, then capture the runtime errors if any, and
  typecheck. We'll report type errors if any, and then either re-throw the
  captured runtime errors or return the resulting value.

  The only other caveat is that if there are parse errors, we want to report
  those immediately before typechecking, because TypeScript won't have received
  the full picture anyway.
*/
export async function typecheck<T>(
  { tsconfigPath }: { tsconfigPath: string },
  fn: (ctx: {
    addSourceFile: (path: string, code: string) => void;
  }) => Promise<T>,
): Promise<T> {
  const project = new Project({
    tsConfigFilePath: tsconfigPath,
    skipAddingFilesFromTsConfig: true,
  });

  const result = await promiseToResult(
    fn({
      addSourceFile: (path, code) =>
        project.createSourceFile(path, code, { overwrite: true }),
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
    (value) => ({ type: "success", value }) as const,
    (error: unknown) => ({ type: "error", error }) as const,
  );
}
