import * as z from "zod";
import type { CheckFn } from "zod/v4/core";
import type { NpmTarget } from "./input-data.ts";
import { UNDEFINED_LIBC_NAME } from "./output-data.ts";
import { isDirPath, pathExists, PathSchema } from "./util.ts";

const PLACEHOLDERS = {
  os: "$os",
  cpu: "$cpu",
  libc: "$libc",
};

export const ARGS_OPTIONS = {
  help: { type: "boolean", short: "h" },
  "input-dir": { type: "string" },
  "output-dir": { type: "string" },
  "main-package-name": { type: "string", default: "@wasp.sh/wasp-cli" },
  "sub-package-name": {
    type: "string",
    default: `@wasp.sh/wasp-cli-${PLACEHOLDERS.os}-${PLACEHOLDERS.cpu}-${PLACEHOLDERS.libc}`,
  },
} as const;

export const ArgsSchema = z.object({
  "input-dir": PathSchema().check(isDirPath).check(pathExists),
  "output-dir": PathSchema().check(isDirPath),
  "main-package-name": z.string().nonempty(),
  "sub-package-name": z
    .string()
    .check(checkIncludesPlaceholder(PLACEHOLDERS.os))
    .check(checkIncludesPlaceholder(PLACEHOLDERS.cpu))
    .check(checkIncludesPlaceholder(PLACEHOLDERS.libc))
    .transform(
      (s) =>
        ({ os, cpu, libc = UNDEFINED_LIBC_NAME }: NpmTarget) =>
          s
            .replace(PLACEHOLDERS.os, os)
            .replace(PLACEHOLDERS.cpu, cpu)
            .replace(PLACEHOLDERS.libc, libc),
    ),
});

function checkIncludesPlaceholder(placeholder: string): CheckFn<string> {
  return (ctx) => {
    if (!ctx.value.includes(placeholder)) {
      ctx.issues.push({
        code: "custom",
        input: ctx.value,
        message: `Must contain the ${placeholder} placeholder`,
      });
    }
  };
}
