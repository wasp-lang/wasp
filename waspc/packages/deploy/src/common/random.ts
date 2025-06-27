import crypto from "crypto";

export function generateRandomString(sizeBytes: number = 32): string {
  return crypto.randomBytes(sizeBytes).toString("hex");
}
