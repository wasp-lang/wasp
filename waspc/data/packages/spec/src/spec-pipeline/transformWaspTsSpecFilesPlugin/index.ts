import assert from "node:assert/strict";
import type { Plugin } from "rolldown";
import { WASP_SPEC_FILE_REGEX } from "../common.js";
import { transformWaspTsSpecFile_mutate } from "./transformWaspTsSpecFile.js";

export function transformWaspTsSpecFilesPlugin(): Plugin {
  return {
    name: "wasp/spec/transform-wasp-ts-spec-files",

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

        transformWaspTsSpecFile_mutate(magicString, {
          sourceText: code,
          sourcePath: id,
        });

        return { code: magicString };
      },
    },
  };
}
