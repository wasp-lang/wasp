import * as z from "zod";
import { isFilePath, type Path, pathExists, PathSchema } from "./util.ts";

const NpmTargetSchema = z.object({
  os: z.string(),
  cpu: z.string(),
  libc: z.string().optional(),
});
export type NpmTarget = z.infer<typeof NpmTargetSchema>;

export const TarballDataSchema = (inputDir: Path) =>
  z.object({
    fileName: PathSchema(inputDir).check(isFilePath).check(pathExists),
    target: NpmTargetSchema,
  });
export type TarballData = z.infer<ReturnType<typeof TarballDataSchema>>;

export const BuildDataSchema = (inputDir: Path) =>
  z.object({
    version: z.string(),
    tarballs: z.array(TarballDataSchema(inputDir)),
  });
export type BuildData = z.infer<ReturnType<typeof BuildDataSchema>>;
