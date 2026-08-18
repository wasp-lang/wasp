/**
 * Normalizes a string identifier for storage and lookup of `email` and
 * `username` auth providers.
 *
 * It applies NFC normalization to avoid duplicate accounts caused by different Unicode encodings,
 * and converts to lowercase for case-insensitive lookup.
 */
export function nfcNormalizeLowercase(input: string): string {
  return input.normalize("NFC").toLowerCase();
}
