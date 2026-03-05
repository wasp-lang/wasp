import fs from "node:fs";
import * as path from "node:path";
import * as z from "zod";
import type { CheckFn } from "zod/v4/core";

export const PathSchema = (from = process.cwd()) =>
  z
    .string()
    .overwrite((p) => path.resolve(from, p))
    .brand<"Path">();
export type Path = z.infer<ReturnType<typeof PathSchema>>;

export const pathExists: CheckFn<Path> = (ctx) => {
  if (!fs.existsSync(ctx.value)) {
    ctx.issues.push({
      code: "custom",
      input: ctx.value,
      message: "Path does not exist.",
    });
  }
};

export const isDirPath = createPathCheckerFn("directory", (stat) =>
  stat.isDirectory(),
);
export const isFilePath = createPathCheckerFn("file", (stat) => stat.isFile());

function createPathCheckerFn(
  kind: string,
  checkValid: (stat: fs.Stats) => boolean,
): CheckFn<Path> {
  return (ctx) => {
    try {
      const stat = fs.statSync(ctx.value);
      const isValid = checkValid(stat);
      if (!isValid) {
        ctx.issues.push({
          code: "custom",
          input: ctx.value,
          message: `Path already exists but is not a ${kind}.`,
        });
      }
    } catch (err: any) {
      // If the error is ENOENT, the path does not exist, which is acceptable
      // (we may want to create the path later), so we don't throw an error.
      if (err.code !== "ENOENT") {
        ctx.issues.push({
          code: "custom",
          input: ctx.value,
          message: `Unable to access path: ${err.message || String(err)}`,
        });
      }
    }
  };
}
