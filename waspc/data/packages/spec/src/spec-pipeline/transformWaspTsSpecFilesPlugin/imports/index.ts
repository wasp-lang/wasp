import assert from "node:assert/strict";
import type { Plugin } from "rolldown";
import { WASP_SPEC_FILE_REGEX } from "../../common.js";
import { applyTransformImportsPlan_mutate } from "./apply.js";
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
        const ast = meta.ast || this.parse(code, { lang: "ts" });

        const importsPlan = planTransformImports(ast, {
          importingFilePath: id,
        });

        applyTransformImportsPlan_mutate(meta.magicString, importsPlan);

        return { code: meta.magicString };
      },
    },
  };
}
