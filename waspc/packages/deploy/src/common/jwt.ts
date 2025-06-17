import crypto from "crypto";

export function generateRandomJwtSecret(): string {
  return crypto.randomBytes(32).toString("hex");
}
