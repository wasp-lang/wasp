import fs from "node:fs";
import * as path from "node:path";
import * as z from "zod";
import type { ParsePayload } from "zod/v4/core";

export const PathSchema = (from = process.cwd()) =>
  z
    .string()
    .overwrite((p) => path.resolve(from, p))
    .brand<"Path">();
export type Path = z.infer<ReturnType<typeof PathSchema>>;

const createStatChecker =
  ({ kind, checkFn }: { kind: string; checkFn: (stat: fs.Stats) => boolean }) =>
  (ctx: ParsePayload<Path>) => {
    try {
      const stat = fs.statSync(ctx.value);
      if (!checkFn(stat)) {
        ctx.issues.push({
          code: "custom",
          input: ctx.value,
          message: `Path already exists but is not a ${kind}.`,
        });
      }
    } catch (err: any) {
      // If the error is ENOENT, the path does not exist, which is acceptable.
      if (err.code !== "ENOENT") {
        ctx.issues.push({
          code: "custom",
          input: ctx.value,
          message: `Unable to access path: ${err.message || String(err)}`,
        });
      }
    }
  };

export const [isDir, isFile] = (
  [
    { kind: "directory", checkFn: (stat) => stat.isDirectory() },
    { kind: "file", checkFn: (stat) => stat.isFile() },
  ] as Parameters<typeof createStatChecker>[0][]
).map(createStatChecker);

export const exists = (ctx: ParsePayload<Path>) => {
  if (!fs.existsSync(ctx.value)) {
    ctx.issues.push({
      code: "custom",
      input: ctx.value,
      message: "Path does not exist.",
    });
  }
};
