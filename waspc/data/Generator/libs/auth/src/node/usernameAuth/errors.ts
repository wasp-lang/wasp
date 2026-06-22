import { AuthServiceError } from "../authService/errors";

export function throwInvalidCredentials(): never {
  throw new AuthServiceError("invalid-credentials", "Invalid credentials");
}
