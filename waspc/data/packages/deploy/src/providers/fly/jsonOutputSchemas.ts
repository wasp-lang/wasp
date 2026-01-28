import * as z from "zod";
import { canonicalSchema, toLowerCaseKeysSchema } from "../../common/schema";

// flyctl plays loose with the casing of the keys in its JSON output, so we
// wrap all schemas with toLowerCaseKeysSchema to accept any casing.

const FlyRegionSchema = toLowerCaseKeysSchema.pipe(
  z.object({
    code: z.string(),
    name: z.string(),
  }),
);

export const FlyRegionListSchema = canonicalSchema(
  // flyctl <v0.3.214
  z.array(FlyRegionSchema),

  [
    // flyctl >=v0.3.214
    toLowerCaseKeysSchema
      .pipe(z.object({ regions: z.unknown() }))
      .transform((data) => data.regions),
  ],
);

const FlySecretSchema = toLowerCaseKeysSchema.pipe(
  z.object({
    name: z.string(),
  }),
);
export const FlySecretListSchema = z.array(FlySecretSchema);
