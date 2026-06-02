import assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import type { Plugin } from "rolldown";
import { WASP_SPEC_FILE_REGEX } from "../common.js";
import { applyLowerImportsPlan_mutate } from "./apply.js";
import { planLowerImports } from "./plan.js";

export async function lowerImportsPlugin({
  projectRootDir,
}: {
  projectRootDir: string;
}): Promise<Plugin> {
  // The bundler resolves symlinks in the module ids, so we resolve our root dir
  // here too. Otherwise we could treat valid in-`src/` imports as escaping
  // `src/` because one path's symlinks are resolved and the other's are not.
  const canonicalProjectRootDir = await fs.realpath(projectRootDir);

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
          projectRootDir: canonicalProjectRootDir,
        });

        applyLowerImportsPlan_mutate(magicString, plan);

        return { code: magicString };
      },
    },
  };
}
