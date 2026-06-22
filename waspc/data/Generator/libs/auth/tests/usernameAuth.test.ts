import { describe, expect, it } from "vitest";
import {
  AuthServiceError,
  getProviderDataWithPassword,
  loginWithUsername,
  sanitizeAndSerializeProviderData,
  signupWithUsername,
  type AuthIdentity,
  type UsernameLoginAdapters,
  type UsernameSignupAdapters,
} from "../src/node";

type RequestContext = { requestId: string };
type User = { id: string };
type CreatedUser = User & { auth: { id: string } | null };
type UserFields = { displayName?: string };

describe("username auth", () => {
  it("should sign up a user and preserve hook ordering", async () => {
    const events: string[] = [];
    let savedProviderData = "";
    const adapters = createSignupAdapters({ events });

    await expect(
      signupWithUsername({
        fields: { username: "MiHo", password: "password123" },
        request: { requestId: "req-id" },
        getUserFields: async () => ({ displayName: "Miho" }),
        adapters,
      }),
    ).resolves.toEqual({ success: true });

    expect(events).toEqual([
      "beforeSignup:miho",
      "createUser:miho:Miho",
      "afterSignup:miho:user-id",
    ]);
    expect(getProviderDataWithPassword<"username">(savedProviderData)).toEqual({
      hashedPassword: expect.not.stringMatching(/^password123$/),
    });

    function createSignupAdapters({
      events,
    }: {
      events: string[];
    }): UsernameSignupAdapters<RequestContext, CreatedUser, UserFields> {
      return {
        authRepository: {
          async createUserWithIdentity({
            providerId,
            serializedProviderData,
            userFields,
          }) {
            savedProviderData = serializedProviderData ?? "";
            events.push(
              `createUser:${providerId.providerUserId}:${userFields?.displayName}`,
            );
            return {
              authId: "auth-id",
              user: { id: "user-id", auth: { id: "auth-id" } },
            };
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
      };
    }
  });

  it("should not call after-signup hook when signup persistence fails", async () => {
    const events: string[] = [];
    const error = new AuthServiceError(
      "identity-already-exists",
      "Identity already exists",
    );

    await expect(
      signupWithUsername({
        fields: { username: "miho", password: "password123" },
        request: { requestId: "req-id" },
        adapters: {
          authRepository: {
            async createUserWithIdentity() {
              events.push("createUser");
              throw error;
            },
          },
          hooks: {
            async onBeforeSignup() {
              events.push("beforeSignup");
            },
            async onAfterSignup() {
              events.push("afterSignup");
            },
          },
        },
      }),
    ).rejects.toBe(error);

    expect(events).toEqual(["beforeSignup", "createUser"]);
  });

  it("should log in a user and preserve hook ordering", async () => {
    const events: string[] = [];
    const adapters = await createLoginAdapters({ events });

    await expect(
      loginWithUsername({
        fields: { username: "MiHo", password: "password123" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).resolves.toEqual({ sessionId: "session-id" });

    expect(events).toEqual([
      "findIdentity:miho",
      "findAuth:auth-id",
      "beforeLogin:miho:user-id",
      "createSession:auth-id",
      "afterLogin:miho:user-id",
    ]);
  });

  it("should reject login when identity is missing", async () => {
    const events: string[] = [];
    const adapters = await createLoginAdapters({ events, identity: null });

    await expect(
      loginWithUsername({
        fields: { username: "miho", password: "password123" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).rejects.toMatchObject({ code: "invalid-credentials" });

    expect(events).toEqual(["findIdentity:miho"]);
  });

  it("should reject login when password verification fails", async () => {
    const events: string[] = [];
    const adapters = await createLoginAdapters({ events });

    await expect(
      loginWithUsername({
        fields: { username: "miho", password: "wrongPassword123" },
        request: { requestId: "req-id" },
        adapters,
      }),
    ).rejects.toMatchObject({ code: "invalid-credentials" });

    expect(events).toEqual(["findIdentity:miho"]);
  });
});

async function createLoginAdapters({
  events,
  identity,
}: {
  events: string[];
  identity?: AuthIdentity<"username"> | null;
}): Promise<UsernameLoginAdapters<RequestContext, User>> {
  const defaultIdentity = await createUsernameIdentity();
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

async function createUsernameIdentity(): Promise<AuthIdentity<"username">> {
  return {
    authId: "auth-id",
    providerName: "username",
    providerUserId: "miho",
    providerData: await sanitizeAndSerializeProviderData<"username">({
      hashedPassword: "password123",
    }),
  };
}
