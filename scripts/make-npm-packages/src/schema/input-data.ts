import * as z from "zod";
import { exists, isFile, type Path, PathSchema } from "./util.ts";

export const TarballDataSchema = (inputDir: Path) =>
  z.object({
    fileName: PathSchema(inputDir).check(isFile).check(exists),
    target: z.tuple([z.string(), z.string(), z.string().optional()]),
  });
export type TarballData = z.infer<ReturnType<typeof TarballDataSchema>>;

export const BuildDataSchema = (inputDir: Path) =>
  z.object({
    version: z.string(),
    tarballs: z.array(TarballDataSchema(inputDir)),
  });
export type BuildData = z.infer<ReturnType<typeof BuildDataSchema>>;
