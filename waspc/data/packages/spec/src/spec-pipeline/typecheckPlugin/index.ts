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

      if (diagnostics.length > 0) {
        console.error(formatDiagnosticsWithColorAndContext(diagnostics));
      }

      if (diagnostics.some((d) => d.category === ts.DiagnosticCategory.Error)) {
        throw new SpecUserError("Type errors found");
      }
    },
  };
}
