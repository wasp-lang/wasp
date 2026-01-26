import crypto from "crypto";

export function generateRandomHexString(sizeBytes: number = 32): string {
  return crypto.randomBytes(sizeBytes).toString("hex");
}
