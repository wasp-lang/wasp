import { describe, expect, it } from "vitest";
import { hashPassword, verifyPassword } from "../../src/sdk/password";

describe("password utilities", () => {
  async function expectHashingToSucceed(password: string): Promise<void> {
    const hashedPassword = await hashPassword(password);

    expect(hashedPassword.length).toBeGreaterThan(0);
    expect(hashedPassword).not.toBe(password);
  }

  describe("hashPassword", () => {
    it("should hash a password and return a string", async () => {
      await expectHashingToSucceed("testPassword123");
    });

    it("should handle special characters", async () => {
      await expectHashingToSucceed("pässwörd!@#$%^&*()");
    });

    it("should handle unicode characters", async () => {
      await expectHashingToSucceed("パスワード🔒");
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
      await expectVerificationToSucceed(hash1, password2);
      await expectVerificationToSucceed(hash2, password1);
    });
  });

  describe("verifyPassword", () => {
    function testPasswordHashVerification(
      scenario: string,
      password: string,
      wrongPassword: string,
    ) {
      it(`should verify password correctly for: ${scenario}`, async () => {
        const hashedPassword = await hashPassword(password);

        await expectVerificationToSucceed(hashedPassword, password);
        await expectVerificationToFail(hashedPassword, wrongPassword);
      });
    }

    testPasswordHashVerification(
      "typical password",
      "correctPassword123",
      "wrongPassword456",
    );

    testPasswordHashVerification("empty password", "", "nonEmpty");

    testPasswordHashVerification(
      "special characters",
      "special!@#$%^&*()",
      "different!@#",
    );

    testPasswordHashVerification(
      "unicode characters",
      "パスワード🔒",
      "different🔓",
    );

    testPasswordHashVerification(
      "long password",
      "a".repeat(1000),
      "a".repeat(999),
    );

    testPasswordHashVerification(
      "case sensitive password",
      "CaseSensitive123",
      "casesensitive123",
    );

    it("should reject when hash is invalid", async () => {
      await expectVerificationToFail(
        "invalid-hash-string",
        "testPassword",
        "Invalid hashed password",
      );
    });
  });
});

async function expectVerificationToSucceed(
  hashedPassword: string,
  password: string,
): Promise<void> {
  return expect(
    verifyPassword(hashedPassword, password),
  ).resolves.not.toThrow();
}

async function expectVerificationToFail(
  hashedPassword: string,
  password: string,
  expectedErrorMessage = "Invalid password",
): Promise<void> {
  return expect(verifyPassword(hashedPassword, password)).rejects.toThrow(
    expectedErrorMessage,
  );
}
