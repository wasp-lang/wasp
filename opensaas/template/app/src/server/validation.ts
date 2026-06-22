import { HttpError } from "wasp/server";
import * as z from "zod";

export function ensureArgsSchemaOrThrowHttpError<Schema extends z.ZodType>(
  schema: Schema,
  rawArgs: unknown,
): z.infer<Schema> {
  const parseResult = schema.safeParse(rawArgs);
  if (!parseResult.success) {
    console.error(
      // We keep the `cause` property so that errors have stack traces pointing
      // to the original schema.
      new Error(
        "Operation arguments validation failed:\n" +
          z.prettifyError(parseResult.error),
        { cause: parseResult.error },
      ),
    );

    throw new HttpError(400, "Operation arguments validation failed", {
      cause: parseResult.error,
    });
  } else {
    return parseResult.data;
  }
}
