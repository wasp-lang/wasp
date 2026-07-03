import assert from "node:assert/strict";
import type { Plugin, RolldownMagicString } from "rolldown";
import type { ESTree as t } from "rolldown/utils";
import { WASP_SPEC_FILE_REGEX } from "../../common.js";
import { applyTransformImportsPlan_mutate } from "./apply.js";
import { assertCanTransformImports } from "./check.js";
import { planTransformImports } from "./plan.js";

export function transformRefImportsPlugin(): Plugin {
  return {
    name: "wasp/spec/transform-ref-imports",

    options(opts) {
      return {
        ...opts,
        experimental: {
          ...opts.experimental,
          // TODO: Remove this once it goes out of experimental phase.
          // https://rolldown.rs/in-depth/native-magic-string
          nativeMagicString: true,
        },
      };
    },

    transform: {
      filter: { id: WASP_SPEC_FILE_REGEX },
      async handler(code, id, meta) {
        assert(meta.magicString);

        // If the AST is already available, use it; otherwise ask the bundler to
        // parse it.
        const ast =
          meta.ast ||
          this.parse(code, { lang: id.endsWith(".tsx") ? "tsx" : "ts" });

        transformRefImports_mutate(ast, meta.magicString);

        return { code: meta.magicString };
      },
    },
  };
}

export function transformRefImports_mutate(
  ast: t.Program,
  magicString: RolldownMagicString,
): void {
  assertCanTransformImports(ast);

  const importsPlan = planTransformImports(ast);

  if (!importsPlan) {
    return;
  }

  applyTransformImportsPlan_mutate(magicString, importsPlan);
}
