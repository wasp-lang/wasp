import type { z } from "zod";
import { asDomainError } from "./errors";

/** Parses service input; validation failures become DomainError("invalid"). */
export function parseInput<S extends z.ZodType>(
  schema: S,
  input: unknown,
): z.output<S> {
  const result = schema.safeParse(input);
  if (!result.success) throw asDomainError(result.error)!;
  return result.data;
}
