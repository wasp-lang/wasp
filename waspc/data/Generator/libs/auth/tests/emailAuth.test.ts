import { describe, expect, it } from "vitest";
import {
  getProviderDataWithPassword,
  loginWithEmail,
  sanitizeAndSerializeProviderData,
  signupWithEmail,
  type AuthIdentity,
  type EmailLoginAdapters,
  type EmailSignupAdapters,
} from "../src/node";

type RequestContext = { requestId: string };
type User = { id: string };
type CreatedUser = User & { auth: { id: string } | null };
type UserFields = { displayName?: string };

const now = new Date("2026-01-01T00:00:00.000Z");

describe("email auth", () => {
  it("should log in a verified user and preserve hook ordering", async () => {
    const events: string[] = [];
    const adapters = await createLoginAdapters({ events });

    await expect(
      loginWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).resolves.toEqual({ sessionId: "session-id" });

    expect(events).toEqual([
      "findIdentity:test@example.com",
      "findAuth:auth-id",
      "beforeLogin:test@example.com:user-id",
      "createSession:auth-id",
      "afterLogin:test@example.com:user-id",
    ]);
  });

  it("should reject login when identity is missing", async () => {
    const events: string[] = [];
    const adapters = await createLoginAdapters({ events, identity: null });

    await expect(
      loginWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).rejects.toMatchObject({ code: "invalid-credentials" });

    expect(events).toEqual(["findIdentity:test@example.com"]);
  });

  it("should reject login when password verification fails", async () => {
    const events: string[] = [];
    const adapters = await createLoginAdapters({ events });

    await expect(
      loginWithEmail({
        fields: { email: "test@example.com", password: "wrongPassword123" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).rejects.toMatchObject({ code: "invalid-credentials" });

    expect(events).toEqual(["findIdentity:test@example.com"]);
  });

  it("should reject login when email is unverified", async () => {
    const events: string[] = [];
    const adapters = await createLoginAdapters({
      events,
      identity: await createEmailIdentity({ isEmailVerified: false }),
    });

    await expect(
      loginWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).rejects.toMatchObject({ code: "invalid-credentials" });

    expect(events).toEqual(["findIdentity:test@example.com"]);
  });

  it("should fake work for duplicate verified signup", async () => {
    const events: string[] = [];
    const adapters = await createSignupAdapters({
      events,
      identity: await createEmailIdentity({ isEmailVerified: true }),
    });

    await expect(
      signupWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        getUserFields: () => {
          events.push("getUserFields");
          return { displayName: "Miho" };
        },
        isEmailAutoVerified: false,
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual(["findIdentity:test@example.com", "fakeWork"]);
  });

  it("should throttle duplicate unverified signup", async () => {
    const events: string[] = [];
    const adapters = await createSignupAdapters({
      events,
      identity: await createEmailIdentity({
        isEmailVerified: false,
        emailVerificationSentAt: now.toISOString(),
      }),
    });

    await expect(
      signupWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        isEmailAutoVerified: false,
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "email-resend-too-soon",
      metadata: { timeLeft: 60 },
    });

    expect(events).toEqual(["findIdentity:test@example.com"]);
  });

  it("should not throttle duplicate unverified signup by password reset timestamp", async () => {
    const events: string[] = [];
    const adapters = await createSignupAdapters({
      events,
      identity: await createEmailIdentity({
        isEmailVerified: false,
        emailVerificationSentAt: null,
        passwordResetSentAt: now.toISOString(),
      }),
    });

    await expect(
      signupWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        isEmailAutoVerified: false,
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toContain("deleteUser:auth-id");
    expect(events).toContain(
      "sendVerificationEmail:test@example.com:https://example.com/verify",
    );
  });

  it("should replace duplicate unverified signup and send verification email", async () => {
    const events: string[] = [];
    let savedProviderData = "";
    const adapters = await createSignupAdapters({
      events,
      setSavedProviderData: (providerData) => {
        savedProviderData = providerData;
      },
      identity: await createEmailIdentity({
        isEmailVerified: false,
        emailVerificationSentAt: null,
      }),
    });

    await expect(
      signupWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        getUserFields: () => {
          events.push("getUserFields");
          return { displayName: "Miho" };
        },
        isEmailAutoVerified: false,
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual([
      "findIdentity:test@example.com",
      "deleteUser:auth-id",
      "getUserFields",
      "beforeSignup:test@example.com",
      "createUser:test@example.com:Miho",
      "afterSignup:test@example.com:user-id",
      "createVerificationLink:test@example.com",
      "sendVerificationEmail:test@example.com:https://example.com/verify",
    ]);
    expect(
      getProviderDataWithPassword<"email">(savedProviderData),
    ).toMatchObject({
      isEmailVerified: false,
      emailVerificationSentAt: null,
      passwordResetSentAt: null,
    });
  });

  it("should skip verification email for auto-verified signup", async () => {
    const events: string[] = [];
    let savedProviderData = "";
    const adapters = await createSignupAdapters({
      events,
      identity: null,
      setSavedProviderData: (providerData) => {
        savedProviderData = providerData;
      },
    });

    await expect(
      signupWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        isEmailAutoVerified: true,
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual([
      "findIdentity:test@example.com",
      "beforeSignup:test@example.com",
      "createUser:test@example.com:none",
      "afterSignup:test@example.com:user-id",
    ]);
    expect(
      getProviderDataWithPassword<"email">(savedProviderData),
    ).toMatchObject({
      isEmailVerified: true,
    });
  });

  it("should report verification email send failure", async () => {
    const events: string[] = [];
    const adapters = await createSignupAdapters({
      events,
      identity: null,
      sendVerificationEmail: async () => {
        events.push("sendVerificationEmail:error");
        throw new Error("email failed");
      },
    });

    await expect(
      signupWithEmail({
        fields: { email: "test@example.com", password: "password123" },
        request: { requestId: "req-id" },
        isEmailAutoVerified: false,
        adapters,
      }),
    ).rejects.toMatchObject({ code: "email-delivery-failed" });

    expect(events).toEqual([
      "findIdentity:test@example.com",
      "beforeSignup:test@example.com",
      "createUser:test@example.com:none",
      "afterSignup:test@example.com:user-id",
      "createVerificationLink:test@example.com",
      "sendVerificationEmail:error",
    ]);
  });
});

async function createLoginAdapters({
  events,
  identity,
}: {
  events: string[];
  identity?: AuthIdentity<"email"> | null;
}): Promise<EmailLoginAdapters<RequestContext, User>> {
  const defaultIdentity = await createEmailIdentity({ isEmailVerified: true });
  return {
    authRepository: {
      async findIdentity(providerId) {
        events.push(`findIdentity:${providerId.providerUserId}`);
        return identity === undefined ? defaultIdentity : identity;
      },
      async findAuthWithUserByAuthId(authId) {
        events.push(`findAuth:${authId}`);
        return { authId, user: { id: "user-id" } };
      },
    },
    sessionService: {
      async createSession(authId) {
        events.push(`createSession:${authId}`);
        return { id: "session-id" };
      },
    },
    hooks: {
      async onBeforeLogin({ providerId, user }) {
        events.push(`beforeLogin:${providerId.providerUserId}:${user.id}`);
      },
      async onAfterLogin({ providerId, user }) {
        events.push(`afterLogin:${providerId.providerUserId}:${user.id}`);
      },
    },
  };
}

async function createSignupAdapters({
  events,
  identity,
  setSavedProviderData = () => {},
  sendVerificationEmail,
}: {
  events: string[];
  identity?: AuthIdentity<"email"> | null;
  setSavedProviderData?: (providerData: string) => void;
  sendVerificationEmail?: EmailSignupAdapters<
    RequestContext,
    CreatedUser,
    UserFields
  >["emailVerification"]["sendVerificationEmail"];
}): Promise<EmailSignupAdapters<RequestContext, CreatedUser, UserFields>> {
  const defaultIdentity = await createEmailIdentity({ isEmailVerified: false });
  return {
    authRepository: {
      async findIdentity(providerId) {
        events.push(`findIdentity:${providerId.providerUserId}`);
        return identity === undefined ? defaultIdentity : identity;
      },
      async createUserWithIdentity({
        providerId,
        serializedProviderData,
        userFields,
      }) {
        setSavedProviderData(serializedProviderData ?? "");
        events.push(
          `createUser:${providerId.providerUserId}:${userFields?.displayName ?? "none"}`,
        );
        return {
          authId: "auth-id",
          user: { id: "user-id", auth: { id: "auth-id" } },
        };
      },
      async deleteUserByAuthId(authId) {
        events.push(`deleteUser:${authId}`);
        return { count: 1 };
      },
    },
    hooks: {
      async onBeforeSignup({ providerId }) {
        events.push(`beforeSignup:${providerId.providerUserId}`);
      },
      async onAfterSignup({ providerId, user }) {
        events.push(`afterSignup:${providerId.providerUserId}:${user.id}`);
      },
    },
    emailVerification: {
      async createVerificationLink(email) {
        events.push(`createVerificationLink:${email}`);
        return "https://example.com/verify";
      },
      sendVerificationEmail:
        sendVerificationEmail ??
        (async ({ email, verificationLink }) => {
          events.push(`sendVerificationEmail:${email}:${verificationLink}`);
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

async function createEmailIdentity({
  emailVerificationSentAt = null,
  isEmailVerified,
  passwordResetSentAt = null,
}: {
  emailVerificationSentAt?: string | null;
  isEmailVerified: boolean;
  passwordResetSentAt?: string | null;
}): Promise<AuthIdentity<"email">> {
  return {
    authId: "auth-id",
    providerName: "email",
    providerUserId: "test@example.com",
    providerData: await sanitizeAndSerializeProviderData<"email">({
      hashedPassword: "password123",
      isEmailVerified,
      emailVerificationSentAt,
      passwordResetSentAt,
    }),
  };
}
