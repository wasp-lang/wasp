import * as z from "zod";

/**
 * Takes a series of schemas representing different versions of the same output
 * and combines them into a single schema that can parse any of the versions.
 *
 * The first schema provided is considered the canonical version, while the rest
 * are considered older versions and forced to transform to the same type.
 *
 * @remark Alternative schemas will be piped in to the canonical schema to
 * ensure consistency. As such, it's recommended that the alternative schemas do
 * as little transformation as possible, and just transform the structure to
 * match the canonical schema's input.
 */
export const canonicalSchema = <
  CanonicalInput,
  Canonical extends z.ZodType<unknown, CanonicalInput>,
  const Alternatives extends readonly z.ZodType<CanonicalInput>[],
>(
  canonicalVersionSchema: Canonical,
  alternativeSchemas: Alternatives,
) =>
  z.union([
    canonicalVersionSchema,
    ...alternativeSchemas.map((schema) => schema.pipe(canonicalVersionSchema)),
  ]);

/** Transforms all keys of an object to lowercase */
export const toLowerCaseKeysSchema = z.looseRecord(
  z.string().toLowerCase(),
  z.unknown(),
);
