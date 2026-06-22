import { describe, expect, it } from "vitest";
import {
  AuthServiceError,
  type AuthServiceAdapters,
} from "../src/node/authServiceAdapters";
import { createProviderId } from "../src/node/providerData";

type RequestContext = { requestId: string };
type User = { id: string; email?: string };
type CreatedUser = User & { auth: { id: string } | null };
type UserFields = { displayName?: string };

describe("auth service adapter contract", () => {
  it("should support fake adapters", async () => {
    const providerId = createProviderId("email", "test@example.com");
    const adapters = createFakeAdapters();

    await expect(
      adapters.authRepository.findIdentity(providerId),
    ).resolves.toMatchObject({
      authId: "auth-id",
      providerName: "email",
      providerUserId: "test@example.com",
    });
    await expect(
      adapters.sessionService.createSession("auth-id"),
    ).resolves.toEqual({ id: "session-id" });
    await expect(adapters.oneTimeCodeStore.createToken("auth-id")).resolves.toBe(
      "one-time-code",
    );
    await expect(
      adapters.hooks.onBeforeSignup({
        request: { requestId: "req-id" },
        providerId,
      }),
    ).resolves.toBeUndefined();
    expect(adapters.clock.now().toISOString()).toBe("2026-01-01T00:00:00.000Z");
    expect(adapters.random.integer({ min: 1, max: 3 })).toBe(2);
  });

  it("should expose standalone auth service errors", () => {
    const error = new AuthServiceError("invalid-credentials", "Invalid login", {
      providerName: "email",
    });

    expect(error).toBeInstanceOf(Error);
    expect(error.code).toBe("invalid-credentials");
    expect(error.metadata).toEqual({ providerName: "email" });
  });
});

function createFakeAdapters(): AuthServiceAdapters<
  RequestContext,
  User,
  CreatedUser,
  UserFields
> {
  return {
    authRepository: {
      async findIdentity(providerId) {
        return {
          authId: "auth-id",
          providerName: providerId.providerName,
          providerUserId: providerId.providerUserId,
          providerData: "{}",
        };
      },
      async findAuthWithUserByAuthId(authId) {
        return { authId, user: { id: "user-id" } };
      },
      async createUserWithIdentity() {
        return {
          authId: "auth-id",
          user: { id: "user-id", auth: { id: "auth-id" } },
        };
      },
      async deleteUserByAuthId() {
        return { count: 1 };
      },
      async updateIdentityProviderData({ providerId }) {
        return {
          authId: "auth-id",
          providerName: providerId.providerName,
          providerUserId: providerId.providerUserId,
          providerData: "{}",
        };
      },
    },
    sessionService: {
      async createSession() {
        return { id: "session-id" };
      },
    },
    hooks: {
      async onBeforeSignup() {},
      async onAfterSignup() {},
      async onAfterEmailVerified() {},
      async onBeforeLogin() {},
      async onAfterLogin() {},
      async onBeforeOAuthRedirect({ url }) {
        return { url };
      },
    },
    emailVerification: {
      async createVerificationLink() {
        return "https://example.com/verify";
      },
      async sendVerificationEmail() {},
    },
    passwordReset: {
      async createPasswordResetLink() {
        return "https://example.com/reset";
      },
      async sendPasswordResetEmail() {},
    },
    emailTokens: {
      async verifyEmailToken() {
        return { email: "test@example.com" };
      },
    },
    oneTimeCodeStore: {
      async createToken() {
        return "one-time-code";
      },
      async verifyToken() {
        return { authId: "auth-id" };
      },
      isUsed() {
        return false;
      },
      markUsed() {},
    },
    oauthRedirects: {
      getRedirectUrlForOneTimeCode() {
        return new URL("https://example.com/oauth/success");
      },
      getFailureRedirectUrl() {
        return new URL("https://example.com/oauth/failure");
      },
    },
    clock: {
      now() {
        return new Date("2026-01-01T00:00:00.000Z");
      },
    },
    random: {
      integer({ min, max }) {
        return Math.floor((min + max) / 2);
      },
    },
    workSimulator: {
      async doFakeWork() {},
    },
  };
}
