import { describe, expect, it } from "vitest";
import { verifyPassword } from "../src/node/password";
import {
  createProviderId,
  getProviderData,
  getProviderDataWithPassword,
  mergeAndSerializeProviderDataUpdates,
  normalizeProviderUserId,
  sanitizeAndSerializeProviderData,
  type EmailProviderData,
} from "../src/node/providerData";

describe("provider data", () => {
  it("should normalize email and username provider user IDs", () => {
    expect(createProviderId("email", "Test@Example.com")).toEqual({
      providerName: "email",
      providerUserId: "test@example.com",
    });
    expect(createProviderId("username", "MiHo")).toEqual({
      providerName: "username",
      providerUserId: "miho",
    });
  });

  it("should preserve OAuth provider user IDs", () => {
    const providerUserId = "OAuthUserID";

    expect(normalizeProviderUserId("google", providerUserId)).toBe(
      providerUserId,
    );
    expect(normalizeProviderUserId("github", providerUserId)).toBe(
      providerUserId,
    );
    expect(normalizeProviderUserId("discord", providerUserId)).toBe(
      providerUserId,
    );
    expect(normalizeProviderUserId("keycloak", providerUserId)).toBe(
      providerUserId,
    );
    expect(normalizeProviderUserId("slack", providerUserId)).toBe(
      providerUserId,
    );
    expect(normalizeProviderUserId("microsoft", providerUserId)).toBe(
      providerUserId,
    );
  });

  it("should hash passwords before serializing provider data", async () => {
    const serializedProviderData = await sanitizeAndSerializeProviderData<"email">(
      createEmailProviderData({ hashedPassword: "password123" }),
    );

    const providerData = getProviderDataWithPassword<"email">(
      serializedProviderData,
    );

    expect(providerData.hashedPassword).not.toBe("password123");
    await expect(
      verifyPassword(providerData.hashedPassword, "password123"),
    ).resolves.not.toThrow();
  });

  it("should hide hashed passwords from user-facing provider data", async () => {
    const serializedProviderData = await sanitizeAndSerializeProviderData<"email">(
      createEmailProviderData({ hashedPassword: "password123" }),
    );

    expect(getProviderData<"email">(serializedProviderData)).toEqual({
      isEmailVerified: false,
      emailVerificationSentAt: null,
      passwordResetSentAt: null,
    });
  });

  it("should merge, hash, and serialize provider data updates", async () => {
    const existingProviderData = createEmailProviderData({
      hashedPassword: "oldPassword123",
      emailVerificationSentAt: "2026-01-01T00:00:00.000Z",
    });

    const serializedProviderData =
      await mergeAndSerializeProviderDataUpdates<"email">(existingProviderData, {
        hashedPassword: "newPassword123",
        isEmailVerified: true,
      });

    const providerData = getProviderDataWithPassword<"email">(
      serializedProviderData,
    );

    expect(providerData).toMatchObject({
      isEmailVerified: true,
      emailVerificationSentAt: "2026-01-01T00:00:00.000Z",
      passwordResetSentAt: null,
    });
    await expect(
      verifyPassword(providerData.hashedPassword, "newPassword123"),
    ).resolves.not.toThrow();
  });

  it("should serialize OAuth provider data", async () => {
    await expect(sanitizeAndSerializeProviderData<"google">({})).resolves.toBe(
      "{}",
    );
  });
});

function createEmailProviderData(
  overrides: Partial<EmailProviderData> = {},
): EmailProviderData {
  return {
    hashedPassword: "password123",
    isEmailVerified: false,
    emailVerificationSentAt: null,
    passwordResetSentAt: null,
    ...overrides,
  };
}
