import * as z from "zod";
import { exists, isDir, PathSchema } from "./util.ts";

export const ARGS_OPTIONS = {
  help: { type: "boolean", short: "h" },
  "input-dir": { type: "string" },
  "output-dir": { type: "string" },
  "main-package-name": { type: "string", default: "@wasp.sh/wasp-cli" },
  "sub-package-name": {
    type: "string",
    default: "@wasp.sh/wasp-cli-$os-$cpu-$libc",
  },
} as const;

const includesPlaceholder = (placeholder: string) =>
  [
    (s: string) => s.includes(placeholder),
    `Must contain the ${placeholder} placeholder`,
  ] as const;

export const ArgsSchema = z.object({
  "input-dir": PathSchema().check(isDir).check(exists),
  "output-dir": PathSchema().check(isDir),
  "main-package-name": z.string().nonempty(),
  "sub-package-name": z
    .string()
    .refine(...includesPlaceholder("$os"))
    .refine(...includesPlaceholder("$cpu"))
    .refine(...includesPlaceholder("$libc"))
    .transform(
      (s) => (os: string, cpu: string, libc: string) =>
        s.replace("$os", os).replace("$cpu", cpu).replace("$libc", libc),
    ),
});
