import {
  createCipheriv,
  createDecipheriv,
  createHash,
  hkdfSync,
  randomBytes,
} from "node:crypto";

const TOKEN_PREFIX = {
  access: "mcp_at_",
  refresh: "mcp_rt_",
  code: "mcp_code_",
} as const;

type TokenKind = keyof typeof TOKEN_PREFIX;

export function sha256Hex(value: string): string {
  return createHash("sha256").update(value).digest("hex");
}

/** PKCE S256: base64url(sha256(verifier)). */
export function pkceChallenge(verifier: string): string {
  return createHash("sha256").update(verifier).digest("base64url");
}

/** JSON with object keys sorted, so equal values always serialize the same way. */
export function canonicalJson(value: unknown): string {
  return JSON.stringify(value, (_key, v) =>
    v && typeof v === "object" && !Array.isArray(v)
      ? Object.fromEntries(
          Object.keys(v)
            .sort()
            .map((k) => [k, (v as Record<string, unknown>)[k]]),
        )
      : v,
  );
}

/** A 32-byte key for one purpose, derived from the server secret (HKDF-SHA256). */
export function deriveKey(secret: string, purpose: string): Buffer {
  return Buffer.from(
    hkdfSync("sha256", secret, "wasp-mcp", `wasp-mcp ${purpose}`, 32),
  );
}

const IV_BYTES = 12;
const TAG_BYTES = 16;

/**
 * Codes and tokens are sealed, not stored: AES-256-GCM over a JSON payload, so
 * they can't be read or forged without the secret. Each kind has its own key,
 * so an access token can never pass as a refresh token or a code.
 */
export function seal(key: Buffer, kind: TokenKind, payload: object): string {
  const iv = randomBytes(IV_BYTES);
  const cipher = createCipheriv("aes-256-gcm", key, iv);
  const body = Buffer.concat([
    cipher.update(JSON.stringify(payload), "utf8"),
    cipher.final(),
  ]);
  return (
    TOKEN_PREFIX[kind] +
    Buffer.concat([iv, body, cipher.getAuthTag()]).toString("base64url")
  );
}

/** The payload, or null for anything tampered with, malformed, or of another kind. */
export function unseal<T>(
  key: Buffer,
  kind: TokenKind,
  token: string,
): T | null {
  const prefix = TOKEN_PREFIX[kind];
  if (!token.startsWith(prefix)) return null;
  const raw = Buffer.from(token.slice(prefix.length), "base64url");
  if (raw.length < IV_BYTES + TAG_BYTES + 1) return null;
  try {
    const decipher = createDecipheriv(
      "aes-256-gcm",
      key,
      raw.subarray(0, IV_BYTES),
    );
    decipher.setAuthTag(raw.subarray(raw.length - TAG_BYTES));
    const json = Buffer.concat([
      decipher.update(raw.subarray(IV_BYTES, raw.length - TAG_BYTES)),
      decipher.final(),
    ]).toString("utf8");
    return JSON.parse(json) as T;
  } catch {
    return null;
  }
}
