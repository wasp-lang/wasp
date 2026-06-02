import assert from "node:assert/strict";
import type { Plugin } from "rolldown";
import { WASP_SPEC_FILE_REGEX } from "../common.js";
import { applyLowerImportsPlan_mutate } from "./apply.js";
import { planLowerImports } from "./plan.js";

export function lowerImportsPlugin({
  projectRootDir,
}: {
  projectRootDir: string;
}): Plugin {
  return {
    name: "wasp/spec/lower-imports",

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
      handler(code, id, { magicString }) {
        assert(magicString);

        const plan = planLowerImports({
          sourceText: code,
          importingFilePath: id,
          projectRootDir,
        });

        applyLowerImportsPlan_mutate(magicString, plan);

        return { code: magicString };
      },
    },
  };
}
