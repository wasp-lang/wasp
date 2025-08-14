import { describe, expect, it } from "vitest";
import { hashPassword, verifyPassword } from "../../src/sdk/password";

describe("password utilities", () => {
  describe("hashPassword", () => {
    it("should hash a password and return a string", async () => {
      const password = "testPassword123";

      const hashedPassword = await hashPassword(password);

      expect(hashedPassword).not.toBe(password);
      expect(hashedPassword.length).toBeGreaterThan(0);
    });

    it("should produce different hashes for the same password (due to salt)", async () => {
      const password = "samePassword";

      const hash1 = await hashPassword(password);
      const hash2 = await hashPassword(password);

      expect(hash1).not.toBe(hash2);
    });

    it("should handle special characters", async () => {
      const password = "pässwörd!@#$%^&*()";

      const hashedPassword = await hashPassword(password);

      expect(hashedPassword).toBeDefined();
      expect(typeof hashedPassword).toBe("string");
    });

    it("should handle unicode characters", async () => {
      const password = "パスワード🔒";

      const hashedPassword = await hashPassword(password);

      expect(hashedPassword).toBeDefined();
      expect(typeof hashedPassword).toBe("string");
    });

    it("should normalize password before hashing", async () => {
      // Test with characters that can be normalized differently.
      const password1 = "café"; // composed é
      const password2 = "cafe\u0301"; // e + combining acute accent

      const hash1 = await hashPassword(password1);
      const hash2 = await hashPassword(password2);

      // Both should verify against each other due to normalization.
      await expect(verifyPassword(hash1, password2)).resolves.not.toThrow();
      await expect(verifyPassword(hash2, password1)).resolves.not.toThrow();
    });
  });

  describe("verifyPassword", () => {
    it("should verify a correct password", async () => {
      const password = "correctPassword123";

      const hashedPassword = await hashPassword(password);

      await expect(
        verifyPassword(hashedPassword, password),
      ).resolves.not.toThrow();
    });

    it("should reject an incorrect password", async () => {
      const correctPassword = "correctPassword123";
      const incorrectPassword = "wrongPassword456";

      const hashedPassword = await hashPassword(correctPassword);

      await expect(
        verifyPassword(hashedPassword, incorrectPassword),
      ).rejects.toThrow("Invalid password");
    });

    it("should reject when hash is invalid", async () => {
      const password = "testPassword";
      const invalidHash = "invalid-hash-string";

      await expect(verifyPassword(invalidHash, password)).rejects.toThrow();
    });

    it("should handle empty password verification", async () => {
      const emptyPassword = "";

      const hashedPassword = await hashPassword(emptyPassword);

      await expect(
        verifyPassword(hashedPassword, emptyPassword),
      ).resolves.not.toThrow();
      await expect(verifyPassword(hashedPassword, "notEmpty")).rejects.toThrow(
        "Invalid password",
      );
    });

    it("should handle special characters verification", async () => {
      const password = "special!@#$%^&*()";

      const hashedPassword = await hashPassword(password);

      await expect(
        verifyPassword(hashedPassword, password),
      ).resolves.not.toThrow();
      await expect(
        verifyPassword(hashedPassword, "different!@#"),
      ).rejects.toThrow("Invalid password");
    });

    it("should handle unicode characters verification", async () => {
      const password = "パスワード🔒";

      const hashedPassword = await hashPassword(password);

      await expect(
        verifyPassword(hashedPassword, password),
      ).resolves.not.toThrow();
      await expect(
        verifyPassword(hashedPassword, "different🔓"),
      ).rejects.toThrow("Invalid password");
    });

    it("should be case sensitive", async () => {
      const password = "CaseSensitive";

      const hashedPassword = await hashPassword(password);

      await expect(
        verifyPassword(hashedPassword, password),
      ).resolves.not.toThrow();
      await expect(
        verifyPassword(hashedPassword, "casesensitive"),
      ).rejects.toThrow("Invalid password");
      await expect(
        verifyPassword(hashedPassword, "CASESENSITIVE"),
      ).rejects.toThrow("Invalid password");
    });

    it("should handle very long passwords", async () => {
      const longPassword = "a".repeat(1000);

      const hashedPassword = await hashPassword(longPassword);

      await expect(
        verifyPassword(hashedPassword, longPassword),
      ).resolves.not.toThrow();
      await expect(
        verifyPassword(hashedPassword, "a".repeat(999)),
      ).rejects.toThrow("Invalid password");
    });
  });
});
