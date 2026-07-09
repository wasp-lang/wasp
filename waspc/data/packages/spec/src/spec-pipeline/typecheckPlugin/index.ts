import assert from "node:assert/strict";
import type { Plugin } from "rolldown";
import ts from "typescript";
import { SpecUserError } from "../../spec/specUserError.js";
import { WASP_SPEC_FILE_REGEX } from "../common.js";
import { typecheckProject } from "./project.js";

export function typecheckPlugin({
  tsconfigPath,
}: {
  tsconfigPath: string;
}): Plugin {
  // The bundler hands us each spec file's (already lowered) source as it parses
  // it. We collect them here and type check them together once the build ends.
  const specFileSources = new Map<string, string>();

  return {
    name: "wasp/spec/typecheck",

    moduleParsed(info) {
      if (!WASP_SPEC_FILE_REGEX.test(info.id)) return;
      assert(info.code);
      specFileSources.set(info.id, info.code);
    },

    buildEnd(err) {
      if (err) {
        return;
      }

      const { diagnostics, formatDiagnosticsWithColorAndContext } =
        typecheckProject({
          tsconfigPath,
          overriddenFiles: specFileSources,
        });

      const formattedDiagnostics =
        diagnostics.length > 0
          ? formatDiagnosticsWithColorAndContext(diagnostics).trimEnd()
          : undefined;

      const hasErrorDiagnostic = diagnostics.some(
        (d) => d.category === ts.DiagnosticCategory.Error,
      );

      if (hasErrorDiagnostic) {
        throw new SpecUserError(formattedDiagnostics);
      } else if (formattedDiagnostics) {
        console.warn(formattedDiagnostics);
      }
    },
  };
}
