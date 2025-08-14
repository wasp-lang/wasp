import { describe, expect, it } from "vitest";
import { hashPassword, verifyPassword } from "../../src/sdk/password";

describe("password utilities", () => {
  describe("hashPassword", () => {
    it("should hash a password and return a string", async () => {
      await exceptHashingToSucceed("testPassword123");
    });

    it("should handle special characters", async () => {
      await exceptHashingToSucceed("pässwörd!@#$%^&*()");
    });

    it("should handle unicode characters", async () => {
      await exceptHashingToSucceed("パスワード🔒");
    });

    it("should produce different hashes for the same password (due to salt)", async () => {
      const password = "samePassword";

      const hash1 = await hashPassword(password);
      const hash2 = await hashPassword(password);

      expect(hash1).not.toBe(hash2);
    });

    it("should normalize password before hashing", async () => {
      // Test with characters that can be normalized differently.
      const password1 = "café"; // composed é
      const password2 = "cafe\u0301"; // e + combining acute accent

      const hash1 = await hashPassword(password1);
      const hash2 = await hashPassword(password2);

      // Both should verify against each other due to normalization.
      expectVerficationToSucceed(hash1, password2);
      expectVerficationToSucceed(hash2, password1);
    });
  });

  describe("verifyPassword", () => {
    it("should verify a correct password", async () => {
      const password = "correctPassword123";

      const hashedPassword = await hashPassword(password);

      expectVerficationToSucceed(hashedPassword, password);
    });

    it("should reject an incorrect password", async () => {
      const correctPassword = "correctPassword123";
      const incorrectPassword = "wrongPassword456";

      const hashedPassword = await hashPassword(correctPassword);

      expectVerficationToFail(hashedPassword, incorrectPassword);
    });

    it("should reject when hash is invalid", async () => {
      const password = "testPassword";
      const invalidHash = "invalid-hash-string";

      expectVerficationToFail(invalidHash, password, "Invalid hashed password");
    });

    it("should handle empty password verification", async () => {
      const emptyPassword = "";

      const hashedPassword = await hashPassword(emptyPassword);

      expectVerficationToSucceed(hashedPassword, emptyPassword);
      expectVerficationToFail(hashedPassword, "notEmpty");
    });

    it("should handle special characters verification", async () => {
      const password = "special!@#$%^&*()";

      const hashedPassword = await hashPassword(password);

      expectVerficationToSucceed(hashedPassword, password);
      expectVerficationToFail(hashedPassword, "different!@#");
    });

    it("should handle unicode characters verification", async () => {
      const password = "パスワード🔒";

      const hashedPassword = await hashPassword(password);

      expectVerficationToSucceed(hashedPassword, password);
      expectVerficationToFail(hashedPassword, "different🔓");
    });

    it("should be case sensitive", async () => {
      const password = "CaseSensitive";

      const hashedPassword = await hashPassword(password);

      expectVerficationToSucceed(hashedPassword, password);
      expectVerficationToFail(hashedPassword, password.toLowerCase());
      expectVerficationToFail(hashedPassword, password.toUpperCase());
    });

    it("should handle very long passwords", async () => {
      const longPassword = "a".repeat(1000);

      const hashedPassword = await hashPassword(longPassword);

      expectVerficationToSucceed(hashedPassword, longPassword);
      expectVerficationToFail(hashedPassword, "a".repeat(999));
    });
  });
});

async function exceptHashingToSucceed(password: string): Promise<void> {
  const hashedPassword = await hashPassword(password);

  expect(hashedPassword.length).toBeGreaterThan(0);
  expect(hashedPassword).not.toBe(password);
}

function expectVerficationToSucceed(
  hashedPassword: string,
  password: string,
): void {
  expect(verifyPassword(hashedPassword, password)).resolves.not.toThrow();
}

function expectVerficationToFail(
  hashedPassword: string,
  password: string,
  expectedErrorMessage = "Invalid password",
): void {
  expect(verifyPassword(hashedPassword, password)).rejects.toThrow(
    expectedErrorMessage,
  );
}
