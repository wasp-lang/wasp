import { DiagnosticCategory, Project } from "ts-morph";
import { SpecUserError } from "../spec/specUserError.js";

export async function typecheck<T>(
  { tsconfigPath }: { tsconfigPath: string },
  fn: (ctx: {
    addSourceFile: (path: string, code: string) => void;
  }) => Promise<T>,
): Promise<T> {
  const project = new Project({
    tsConfigFilePath: tsconfigPath,
    skipAddingFilesFromTsConfig: true,
    defaultCompilerOptions: {
      // For some reason `ts-morph` doesn't pick up `@types/*` packages if we
      // don't explicitly set this. (important for `@types/node` globals)
      types: ["*"],
    },
  });

  const loadedWaspTsFiles = new Set<string>();
  const result = await promiseToResult(
    fn({
      addSourceFile: (path, code) => {
        const sourceFile = project.createSourceFile(path, code, {
          overwrite: true,
        });
        if (isWaspTsPath(path)) {
          loadedWaspTsFiles.add(sourceFile.getFilePath());
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

  // Now that all the files are added, we can let TypeScript add every other
  // file in the project and resolve their imports. Ref imports have been
  // lowered so the typechecker won't follow them.
  project.addSourceFilesFromTsConfig(tsconfigPath);
  // Only `.wasp.ts` files reachable from the loaded spec have had their ref
  // imports lowered; other `.wasp.ts` files matched by the tsconfig include
  // patterns still contain unlowered ref imports and would produce spurious
  // type errors, so drop them before resolving dependencies.
  for (const sourceFile of project.getSourceFiles()) {
    const path = sourceFile.getFilePath();
    if (isWaspTsPath(path) && !loadedWaspTsFiles.has(path)) {
      project.removeSourceFile(sourceFile);
    }
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

function isWaspTsPath(path: string): boolean {
  return path.endsWith(".wasp.ts");
}

function promiseToResult<T>(
  promise: Promise<T>,
): Promise<{ type: "success"; value: T } | { type: "error"; error: unknown }> {
  return promise.then(
    (value) => ({ type: "success", value }),
    (error) => ({ type: "error", error }),
  );
}
