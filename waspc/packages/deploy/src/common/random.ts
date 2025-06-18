import crypto from "crypto";

export function generateRandomString(): string {
  return crypto.randomBytes(32).toString("hex");
}
