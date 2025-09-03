import * as z from "zod";

export const FlyRegionListSchema = z.union([
  // older version
  z.array(
    z.object({
      Code: z.string(),
      Name: z.string(),
    }),
  ),
  // newer version as of flyctl v0.3.121
  z.array(
    z.object({
      code: z.string(),
      name: z.string(),
    }),
  ),
]);

export const FlySecretListSchema = z.union([
  // current version
  z.array(
    z.object({
      Name: z.string(),
    }),
  ),
  // not the output yet, but just in case they update the command in the future
  z.array(
    z.object({
      name: z.string(),
    }),
  ),
]);
