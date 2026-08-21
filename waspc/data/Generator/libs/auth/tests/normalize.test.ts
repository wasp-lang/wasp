import { describe, expect, it } from "vitest";
import { nfcNormalizeLowercase } from "../src/normalize";

describe("nfcNormalizeLowercase", () => {
  describe("lowercasing", () => {
    it("lowercases ASCII input", () => {
      expect(nfcNormalizeLowercase("USER")).toBe("user");
      expect(nfcNormalizeLowercase("User@Example.COM")).toBe("user@example.com");
    });

    it("leaves already-lower ASCII input unchanged", () => {
      expect(nfcNormalizeLowercase("user@example.com")).toBe("user@example.com");
    });

    it("is idempotent", () => {
      const once = nfcNormalizeLowercase("User@Example.COM");
      const twice = nfcNormalizeLowercase(once);
      expect(twice).toBe(once);
    });
  });

  describe("NFC normalization", () => {
    it("collapses decomposed local-part diacritics to their precomposed form", () => {
      const decomposed = "jose\u0301.silva@gmail.com";
      const precomposed = "josé.silva@gmail.com";

      expect(nfcNormalizeLowercase(decomposed)).toBe(nfcNormalizeLowercase(precomposed));
      expect(nfcNormalizeLowercase(decomposed)).toBe("josé.silva@gmail.com");
    });

    it("collapses decomposed diacritics in domain characters too", () => {
      const decomposedDomain = "jürgen@mu\u0308nchen.de";
      const precomposedDomain = "jürgen@münchen.de";

      expect(nfcNormalizeLowercase(decomposedDomain)).toBe(
        nfcNormalizeLowercase(precomposedDomain),
      );
      expect(nfcNormalizeLowercase(decomposedDomain)).toBe("jürgen@münchen.de");
    });

    it("treats uppercase NFC and uppercase NFD inputs as the same identifier end-to-end", () => {
      const nfc = "Jürgen@München.de";
      const nfd = "Ju\u0308rgen@Mu\u0308nchen.de";

      expect(nfcNormalizeLowercase(nfc)).toBe(nfcNormalizeLowercase(nfd));
    });

    it("leaves strings with no decomposable characters unchanged (after lowercasing)", () => {
      expect(nfcNormalizeLowercase("café")).toBe("café");
      expect(nfcNormalizeLowercase("PASSWORD123")).toBe("password123");
    });

    it("handles empty strings", () => {
      expect(nfcNormalizeLowercase("")).toBe("");
    });
  });
});
