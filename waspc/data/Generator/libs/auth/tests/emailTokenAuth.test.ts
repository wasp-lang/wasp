import { describe, expect, it } from "vitest";
import { ValidationError } from "../src";
import {
  requestPasswordReset,
  resetPassword,
  verifyEmail,
  type AuthIdentity,
  type EmailPasswordResetAdapters,
  type EmailPasswordResetRequestAdapters,
  type EmailProviderData,
  type EmailVerificationAdapters,
} from "../src/node";

type RequestContext = { requestId: string };
type User = { id: string };

const now = new Date("2026-01-01T00:00:00.000Z");

describe("email token auth", () => {
  it("should fake work when requesting password reset for a missing identity", async () => {
    const events: string[] = [];
    const adapters = createPasswordResetRequestAdapters({
      events,
      identity: null,
    });

    await expect(
      requestPasswordReset({
        fields: { email: "missing@example.com" },
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual(["findIdentity:missing@example.com", "fakeWork"]);
  });

  it("should throttle password reset request resend", async () => {
    const events: string[] = [];
    const adapters = createPasswordResetRequestAdapters({
      events,
      identity: createEmailIdentity({ passwordResetSentAt: now.toISOString() }),
    });

    await expect(
      requestPasswordReset({
        fields: { email: "test@example.com" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "email-resend-too-soon",
      metadata: { timeLeft: 60 },
    });

    expect(events).toEqual(["findIdentity:test@example.com"]);
  });

  it("should create a password reset link and send email to the stored provider user ID", async () => {
    const events: string[] = [];
    const adapters = createPasswordResetRequestAdapters({ events });

    await expect(
      requestPasswordReset({
        fields: { email: "test@example.com" },
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual([
      "findIdentity:test@example.com",
      "createPasswordResetLink:test@example.com",
      "sendPasswordResetEmail:test@example.com:https://example.com/reset",
    ]);
  });

  it("should report password reset email delivery failure", async () => {
    const events: string[] = [];
    const adapters = createPasswordResetRequestAdapters({
      events,
      sendPasswordResetEmail: async () => {
        events.push("sendPasswordResetEmail:error");
        throw new Error("email failed");
      },
    });

    await expect(
      requestPasswordReset({
        fields: { email: "test@example.com" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "email-delivery-failed",
      metadata: {
        logMessage: "Failed to send password reset email:",
        responseMessage: "Failed to send password reset email.",
      },
    });

    expect(events).toEqual([
      "findIdentity:test@example.com",
      "createPasswordResetLink:test@example.com",
      "sendPasswordResetEmail:error",
    ]);
  });

  it("should reset password and verify email for a valid token", async () => {
    const events: string[] = [];
    let savedProviderData: Partial<EmailProviderData> = {};
    const adapters = createPasswordResetAdapters({
      events,
      setSavedProviderData: (providerData) => {
        savedProviderData = providerData;
      },
    });

    await expect(
      resetPassword({
        fields: { token: "valid-token", password: "newPassword123" },
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual([
      "verifyEmailToken:valid-token",
      "findIdentity:test@example.com",
      "updateIdentity:test@example.com",
    ]);
    expect(savedProviderData).toMatchObject({
      isEmailVerified: true,
      hashedPassword: "newPassword123",
    });
  });

  it("should reject password reset when token is missing", async () => {
    const adapters = createPasswordResetAdapters({ events: [] });

    await expect(
      resetPassword({
        fields: { password: "newPassword123" },
        adapters,
      }),
    ).rejects.toBeInstanceOf(ValidationError);
  });

  it("should reject password reset when token is invalid", async () => {
    const events: string[] = [];
    const adapters = createPasswordResetAdapters({
      events,
      verifyEmailToken: async (token) => {
        events.push(`verifyEmailToken:${token}`);
        throw new Error("invalid token");
      },
    });

    await expect(
      resetPassword({
        fields: { token: "invalid-token", password: "newPassword123" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "invalid-token",
      metadata: { responseMessage: "Password reset failed, invalid token" },
    });

    expect(events).toEqual(["verifyEmailToken:invalid-token"]);
  });

  it("should reject password reset when token is expired", async () => {
    const events: string[] = [];
    const adapters = createPasswordResetAdapters({
      events,
      verifyEmailToken: async (token) => {
        events.push(`verifyEmailToken:${token}`);
        throw new Error("expired token");
      },
    });

    await expect(
      resetPassword({
        fields: { token: "expired-token", password: "newPassword123" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "invalid-token",
      metadata: { responseMessage: "Password reset failed, invalid token" },
    });

    expect(events).toEqual(["verifyEmailToken:expired-token"]);
  });

  it("should preserve reusable password reset token behavior", async () => {
    const events: string[] = [];
    const adapters = createPasswordResetAdapters({ events });

    await resetPassword({
      fields: { token: "valid-token", password: "newPassword123" },
      adapters,
    });
    await resetPassword({
      fields: { token: "valid-token", password: "newPassword123" },
      adapters,
    });

    expect(events).toEqual([
      "verifyEmailToken:valid-token",
      "findIdentity:test@example.com",
      "updateIdentity:test@example.com",
      "verifyEmailToken:valid-token",
      "findIdentity:test@example.com",
      "updateIdentity:test@example.com",
    ]);
  });

  it("should verify email and run hook after updating provider data", async () => {
    const events: string[] = [];
    let savedProviderData: Partial<EmailProviderData> = {};
    const adapters = createEmailVerificationAdapters({
      events,
      setSavedProviderData: (providerData) => {
        savedProviderData = providerData;
      },
    });

    await expect(
      verifyEmail({
        fields: { token: "valid-token" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual([
      "verifyEmailToken:valid-token",
      "findIdentity:test@example.com",
      "updateIdentity:test@example.com",
      "findAuth:auth-id",
      "afterEmailVerified:test@example.com:user-id",
    ]);
    expect(savedProviderData).toMatchObject({ isEmailVerified: true });
  });

  it("should reject email verification when token is missing", async () => {
    const events: string[] = [];
    const adapters = createEmailVerificationAdapters({
      events,
      verifyEmailToken: async (token) => {
        events.push(`verifyEmailToken:${String(token)}`);
        throw new Error("missing token");
      },
    });

    await expect(
      verifyEmail({
        fields: {},
        request: { requestId: "req-id" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "invalid-token",
      metadata: { responseMessage: "Email verification failed, invalid token" },
    });

    expect(events).toEqual(["verifyEmailToken:undefined"]);
  });

  it("should reject email verification when token is invalid", async () => {
    const events: string[] = [];
    const adapters = createEmailVerificationAdapters({
      events,
      verifyEmailToken: async (token) => {
        events.push(`verifyEmailToken:${token}`);
        throw new Error("invalid token");
      },
    });

    await expect(
      verifyEmail({
        fields: { token: "invalid-token" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "invalid-token",
      metadata: { responseMessage: "Email verification failed, invalid token" },
    });

    expect(events).toEqual(["verifyEmailToken:invalid-token"]);
  });
});

function createPasswordResetRequestAdapters({
  events,
  identity = createEmailIdentity(),
  sendPasswordResetEmail,
}: {
  events: string[];
  identity?: AuthIdentity<"email"> | null;
  sendPasswordResetEmail?: EmailPasswordResetRequestAdapters["passwordReset"]["sendPasswordResetEmail"];
}): EmailPasswordResetRequestAdapters {
  return {
    authRepository: {
      async findIdentity(providerId) {
        events.push(`findIdentity:${providerId.providerUserId}`);
        return identity;
      },
    },
    passwordReset: {
      async createPasswordResetLink(email) {
        events.push(`createPasswordResetLink:${email}`);
        return "https://example.com/reset";
      },
      sendPasswordResetEmail:
        sendPasswordResetEmail ??
        (async ({ email, passwordResetLink }) => {
          events.push(`sendPasswordResetEmail:${email}:${passwordResetLink}`);
        }),
    },
    clock: {
      now() {
        return now;
      },
    },
    workSimulator: {
      async doFakeWork() {
        events.push("fakeWork");
      },
    },
  };
}

function createPasswordResetAdapters({
  events,
  identity = createEmailIdentity(),
  verifyEmailToken,
  setSavedProviderData = () => {},
}: {
  events: string[];
  identity?: AuthIdentity<"email"> | null;
  verifyEmailToken?: EmailPasswordResetAdapters["tokenService"]["verifyEmailToken"];
  setSavedProviderData?: (providerData: Partial<EmailProviderData>) => void;
}): EmailPasswordResetAdapters {
  return {
    tokenService: {
      verifyEmailToken:
        verifyEmailToken ??
        (async (token) => {
          events.push(`verifyEmailToken:${token}`);
          return { email: "test@example.com" };
        }),
    },
    authRepository: {
      async findIdentity(providerId) {
        events.push(`findIdentity:${providerId.providerUserId}`);
        return identity;
      },
      async updateIdentityProviderData({ providerId, providerDataUpdates }) {
        events.push(`updateIdentity:${providerId.providerUserId}`);
        setSavedProviderData(providerDataUpdates);
        return createEmailIdentity({ providerDataUpdates });
      },
    },
  };
}

function createEmailVerificationAdapters({
  events,
  identity = createEmailIdentity(),
  verifyEmailToken,
  setSavedProviderData = () => {},
}: {
  events: string[];
  identity?: AuthIdentity<"email"> | null;
  verifyEmailToken?: EmailVerificationAdapters<
    RequestContext,
    User
  >["tokenService"]["verifyEmailToken"];
  setSavedProviderData?: (providerData: Partial<EmailProviderData>) => void;
}): EmailVerificationAdapters<RequestContext, User> {
  return {
    tokenService: {
      verifyEmailToken:
        verifyEmailToken ??
        (async (token) => {
          events.push(`verifyEmailToken:${token}`);
          return { email: "test@example.com" };
        }),
    },
    authRepository: {
      async findIdentity(providerId) {
        events.push(`findIdentity:${providerId.providerUserId}`);
        return identity;
      },
      async updateIdentityProviderData({ providerId, providerDataUpdates }) {
        events.push(`updateIdentity:${providerId.providerUserId}`);
        setSavedProviderData(providerDataUpdates);
        return createEmailIdentity({ providerDataUpdates });
      },
      async findAuthWithUserByAuthId(authId) {
        events.push(`findAuth:${authId}`);
        return { authId, user: { id: "user-id" } };
      },
    },
    hooks: {
      async onAfterEmailVerified({ email, user }) {
        events.push(`afterEmailVerified:${email}:${user.id}`);
      },
    },
  };
}

function createEmailIdentity({
  passwordResetSentAt = null,
  isEmailVerified = false,
  providerDataUpdates = {},
}: {
  passwordResetSentAt?: string | null;
  isEmailVerified?: boolean;
  providerDataUpdates?: Partial<EmailProviderData>;
} = {}): AuthIdentity<"email"> {
  return {
    authId: "auth-id",
    providerName: "email",
    providerUserId: "test@example.com",
    providerData: JSON.stringify({
      hashedPassword: "password123",
      isEmailVerified,
      emailVerificationSentAt: null,
      passwordResetSentAt,
      ...providerDataUpdates,
    }),
  };
}
