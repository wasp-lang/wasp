import assert from "node:assert/strict";
import type { Plugin } from "rolldown";
import { WASP_SPEC_FILE_REGEX } from "../../common.js";
import { applyTransformRefHelperPlan_mutate } from "./apply.js";
import { planTransformRefHelper } from "./plan.js";

export function transformRefHelperPlugin(): Plugin {
  return {
    name: "wasp/spec/transform-ref-helper",

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
      handler(code, id, meta) {
        assert(meta.magicString);

        // If the AST is already available, use it; otherwise ask the bundler to
        // parse it.
        const ast = meta.ast || this.parse(code, { lang: "ts" });

        const refHelperPlan = planTransformRefHelper(ast);
        if (!refHelperPlan) {
          return null;
        }

        applyTransformRefHelperPlan_mutate(meta.magicString, refHelperPlan);

        return { code: meta.magicString };
      },
    },
  };
}
