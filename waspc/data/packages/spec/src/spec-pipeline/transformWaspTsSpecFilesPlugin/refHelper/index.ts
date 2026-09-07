import assert from "node:assert/strict";
import type { Plugin, RolldownMagicString } from "rolldown";
import type { ESTree as t } from "rolldown/utils";
import type { RefOrigin } from "../../../spec/refObject.js";
import { WASP_SPEC_FILE_REGEX } from "../../common.js";
import { applyTransformRefHelperPlan_mutate } from "./apply.js";
import { assertCanTransformRefHelper } from "./check.js";
import { planTransformRefHelper } from "./plan.js";

export type GetRefOrigin = (specFilePath: string) => RefOrigin;

export function transformRefHelperPlugin({
  getRefOrigin,
}: {
  getRefOrigin: GetRefOrigin;
}): Plugin {
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

        transformRefHelper_mutate(getRefOrigin(id), ast, meta.magicString);

        return { code: meta.magicString };
      },
    },
  };
}

export function transformRefHelper_mutate(
  origin: RefOrigin,
  ast: t.Program,
  magicString: RolldownMagicString,
): void {
  assertCanTransformRefHelper(ast);

  const refHelperPlan = planTransformRefHelper(ast);
  if (!refHelperPlan) {
    return;
  }

  applyTransformRefHelperPlan_mutate(origin, magicString, refHelperPlan);
}
