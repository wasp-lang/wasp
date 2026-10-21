import { HttpError } from "@wasp.sh/lib-sdk-core";

export function createInvalidCredentialsError(message?: string): HttpError {
  return new HttpError(401, "Invalid credentials", { message });
}

export function throwValidationError(message: string): void {
  throw new HttpError(422, "Validation failed", { message });
}
