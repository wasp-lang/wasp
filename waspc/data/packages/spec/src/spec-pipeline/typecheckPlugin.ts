import assert from "node:assert/strict";
import type { Plugin } from "rolldown";
import { DiagnosticCategory, Project } from "ts-morph";
import { SpecUserError } from "../spec/specUserError.js";
import { WASP_SPEC_FILE_REGEX } from "./common.js";

export function typecheckPlugin({
  tsconfigPath,
}: {
  tsconfigPath: string;
}): Plugin {
  const project = new Project({
    tsConfigFilePath: tsconfigPath ?? undefined,
    skipAddingFilesFromTsConfig: true,
    defaultCompilerOptions: {
      // For some reason `ts-morph` doesn't pick up `@types/*` packages if we
      // don't explicitly set this. (important for `@types/node` globals)
      types: ["*"],
    },
  });

  return {
    name: "wasp/spec/typecheck",
    moduleParsed(info) {
      if (!WASP_SPEC_FILE_REGEX.test(info.id)) return;
      assert(info.code);
      project.createSourceFile(info.id, info.code, { overwrite: true });
    },
    buildEnd(err) {
      if (err) return;
      project.resolveSourceFileDependencies();
      const diagnostics = project.getPreEmitDiagnostics();

      if (diagnostics.length > 0) {
        console.error(
          project.formatDiagnosticsWithColorAndContext(diagnostics),
        );
      }

      if (
        diagnostics.some((d) => d.getCategory() === DiagnosticCategory.Error)
      ) {
        throw new SpecUserError("Type errors found");
      }
    },
  };
}
