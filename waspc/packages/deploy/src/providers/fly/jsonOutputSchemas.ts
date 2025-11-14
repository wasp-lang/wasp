import * as z from "zod";

const FlyRegionSchema = z.object({
  code: z.string(),
  name: z.string(),
});

const FlyRegionSchemaLegacy = z.object({
  Code: z.string(),
  Name: z.string(),
});

export const FlyRegionListSchema = z.union([
  // older version (capitalized fields)
  z.array(FlyRegionSchemaLegacy),
  // version as of flyctl v0.3.121 (lowercase fields, array format)
  z.array(FlyRegionSchema),
  // version as of flyctl v0.3.214 (object with Regions field)
  z.object({
    Regions: z.array(FlyRegionSchema),
  }),
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
