import { describe, expect, it } from "vitest";
import {
  completeOAuthCallback,
  exchangeOAuthCodeForSession,
  getOAuthCallbackErrorRedirectUrl,
  type AuthIdentity,
  type OAuthCallbackAdapters,
  type OAuthCodeExchangeAdapters,
} from "../src/node";

type RequestContext = { requestId: string };
type User = { id: string };
type CreatedUser = User & { auth: { id: string } | null };
type UserFields = { displayName?: string };
type OAuthData = {
  uniqueRequestId: string;
  providerName: "google";
  tokens: { accessToken: string };
};

const oauth: OAuthData = {
  uniqueRequestId: "state-id",
  providerName: "google",
  tokens: { accessToken: "access-token" },
};

describe("oauth auth", () => {
  it("should complete callback for an existing user and preserve hook ordering", async () => {
    const events: string[] = [];
    const adapters = createCallbackAdapters({ events });

    await expect(
      completeOAuthCallback({
        providerName: "google",
        providerUserId: "provider-user-id",
        providerProfile: { email: "test@example.com" },
        request: { requestId: "req-id" },
        oauth,
        adapters,
      }),
    ).resolves.toEqual({
      redirectUrl: new URL("https://example.com/oauth/callback#one-time-code"),
    });

    expect(events).toEqual([
      "findIdentity:google:provider-user-id",
      "findAuth:auth-id",
      "beforeLogin:google:provider-user-id:user-id",
      "afterLogin:google:provider-user-id:user-id:state-id",
      "createToken:auth-id",
      "redirect:one-time-code",
    ]);
  });

  it("should complete callback for a new user and preserve hook ordering", async () => {
    const events: string[] = [];
    let savedProviderData = "";
    const adapters = createCallbackAdapters({
      events,
      identity: null,
      setSavedProviderData: (providerData) => {
        savedProviderData = providerData;
      },
    });

    await expect(
      completeOAuthCallback({
        providerName: "google",
        providerUserId: "provider-user-id",
        providerProfile: { email: "test@example.com" },
        request: { requestId: "req-id" },
        oauth,
        getUserFields: (providerProfile) => {
          events.push(`getUserFields:${JSON.stringify(providerProfile)}`);
          return { displayName: "Miho" };
        },
        adapters,
      }),
    ).resolves.toEqual({
      redirectUrl: new URL("https://example.com/oauth/callback#one-time-code"),
    });

    expect(events).toEqual([
      "findIdentity:google:provider-user-id",
      'getUserFields:{"email":"test@example.com"}',
      "beforeSignup:google:provider-user-id",
      "createUser:google:provider-user-id:Miho",
      "afterSignup:google:provider-user-id:user-id:state-id",
      "createToken:auth-id",
      "redirect:one-time-code",
    ]);
    expect(JSON.parse(savedProviderData)).toEqual({});
  });

  it("should build provider token failure redirect", () => {
    const events: string[] = [];
    const error = new Error("token failed");

    expect(
      getOAuthCallbackErrorRedirectUrl({
        error,
        adapters: createErrorRedirectAdapters(events),
      }),
    ).toEqual({
      redirectUrl: new URL("https://example.com/oauth/callback?error=token"),
    });
    expect(events).toEqual(["failureRedirect:token failed"]);
  });

  it("should build provider profile failure redirect", () => {
    const events: string[] = [];
    const error = new Error("profile failed");

    expect(
      getOAuthCallbackErrorRedirectUrl({
        error,
        adapters: createErrorRedirectAdapters(events),
      }),
    ).toEqual({
      redirectUrl: new URL("https://example.com/oauth/callback?error=profile"),
    });
    expect(events).toEqual(["failureRedirect:profile failed"]);
  });

  it("should exchange a valid one-time code for a session", async () => {
    const events: string[] = [];
    const adapters = createCodeExchangeAdapters({ events });

    await expect(
      exchangeOAuthCodeForSession({
        fields: { code: "valid-code" },
        adapters,
      }),
    ).resolves.toEqual({ sessionId: "session-id" });

    expect(events).toEqual([
      "isUsed:valid-code",
      "verifyToken:valid-code",
      "findAuth:auth-id",
      "createSession:auth-id",
      "markUsed:valid-code",
    ]);
  });

  it("should reject one-time code exchange when code is missing", async () => {
    const events: string[] = [];
    const adapters = createCodeExchangeAdapters({ events });

    await expect(
      exchangeOAuthCodeForSession({ fields: {}, adapters }),
    ).rejects.toMatchObject({
      code: "missing-token",
      metadata: {
        responseMessage:
          "Unable to login with the OAuth provider. The code is missing.",
      },
    });
    expect(events).toEqual([]);
  });

  it("should reject one-time code exchange when code was already used", async () => {
    const events: string[] = [];
    const adapters = createCodeExchangeAdapters({ events, isUsed: true });

    await expect(
      exchangeOAuthCodeForSession({
        fields: { code: "used-code" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "used-token",
      metadata: {
        responseMessage:
          "Unable to login with the OAuth provider. The code has already been used.",
      },
    });
    expect(events).toEqual(["isUsed:used-code"]);
  });

  it("should reject one-time code exchange when token verification fails", async () => {
    const events: string[] = [];
    const adapters = createCodeExchangeAdapters({
      events,
      verifyToken: async (code) => {
        events.push(`verifyToken:${code}`);
        throw new Error("invalid token");
      },
    });

    await expect(
      exchangeOAuthCodeForSession({
        fields: { code: "invalid-code" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "invalid-token",
      metadata: {
        responseMessage:
          "Unable to login with the OAuth provider. The code is invalid.",
      },
    });
    expect(events).toEqual(["isUsed:invalid-code", "verifyToken:invalid-code"]);
  });

  it("should reject one-time code exchange when auth is missing", async () => {
    const events: string[] = [];
    const adapters = createCodeExchangeAdapters({ events, auth: null });

    await expect(
      exchangeOAuthCodeForSession({
        fields: { code: "invalid-code" },
        adapters,
      }),
    ).rejects.toMatchObject({
      code: "invalid-token",
      metadata: {
        responseMessage:
          "Unable to login with the OAuth provider. The code is invalid.",
      },
    });
    expect(events).toEqual([
      "isUsed:invalid-code",
      "verifyToken:invalid-code",
      "findAuth:auth-id",
    ]);
  });
});

function createCallbackAdapters({
  events,
  identity = createOAuthIdentity(),
  setSavedProviderData = () => {},
}: {
  events: string[];
  identity?: AuthIdentity<"google"> | null;
  setSavedProviderData?: (providerData: string) => void;
}): OAuthCallbackAdapters<
  RequestContext,
  User,
  CreatedUser,
  UserFields,
  OAuthData
> {
  return {
    authRepository: {
      async findIdentity(providerId) {
        events.push(
          `findIdentity:${providerId.providerName}:${providerId.providerUserId}`,
        );
        return identity;
      },
      async findAuthWithUserByAuthId(authId) {
        events.push(`findAuth:${authId}`);
        return { authId, user: { id: "user-id" } };
      },
      async createUserWithIdentity({
        providerId,
        serializedProviderData,
        userFields,
      }) {
        setSavedProviderData(serializedProviderData ?? "");
        events.push(
          `createUser:${providerId.providerName}:${providerId.providerUserId}:${userFields?.displayName ?? "none"}`,
        );
        return {
          authId: "auth-id",
          user: { id: "user-id", auth: { id: "auth-id" } },
        };
      },
    },
    hooks: {
      async onBeforeLogin({ providerId, user }) {
        events.push(
          `beforeLogin:${providerId.providerName}:${providerId.providerUserId}:${user.id}`,
        );
      },
      async onAfterLogin({ providerId, user, oauth }) {
        events.push(
          `afterLogin:${providerId.providerName}:${providerId.providerUserId}:${user.id}:${oauth?.uniqueRequestId}`,
        );
      },
      async onBeforeSignup({ providerId }) {
        events.push(
          `beforeSignup:${providerId.providerName}:${providerId.providerUserId}`,
        );
      },
      async onAfterSignup({ providerId, user, oauth }) {
        events.push(
          `afterSignup:${providerId.providerName}:${providerId.providerUserId}:${user.id}:${oauth?.uniqueRequestId}`,
        );
      },
    },
    oneTimeCodeStore: {
      async createToken(authId) {
        events.push(`createToken:${authId}`);
        return "one-time-code";
      },
    },
    oauthRedirects: {
      getRedirectUrlForOneTimeCode(code) {
        events.push(`redirect:${code}`);
        return new URL(`https://example.com/oauth/callback#${code}`);
      },
    },
  };
}

function createErrorRedirectAdapters(events: string[]) {
  return {
    oauthRedirects: {
      getFailureRedirectUrl(error: unknown) {
        const message = error instanceof Error ? error.message : String(error);
        events.push(`failureRedirect:${message}`);
        const kind = message.includes("profile") ? "profile" : "token";
        return new URL(`https://example.com/oauth/callback?error=${kind}`);
      },
    },
  };
}

function createCodeExchangeAdapters({
  events,
  isUsed = false,
  auth = { authId: "auth-id", user: { id: "user-id" } },
  verifyToken,
}: {
  events: string[];
  isUsed?: boolean;
  auth?: { authId: string; user: User } | null;
  verifyToken?: OAuthCodeExchangeAdapters<User>["oneTimeCodeStore"]["verifyToken"];
}): OAuthCodeExchangeAdapters<User> {
  return {
    oneTimeCodeStore: {
      isUsed(code) {
        events.push(`isUsed:${code}`);
        return isUsed;
      },
      verifyToken:
        verifyToken ??
        (async (code) => {
          events.push(`verifyToken:${code}`);
          return { authId: "auth-id" };
        }),
      markUsed(code) {
        events.push(`markUsed:${code}`);
      },
    },
    authRepository: {
      async findAuthWithUserByAuthId(authId) {
        events.push(`findAuth:${authId}`);
        return auth;
      },
    },
    sessionService: {
      async createSession(authId) {
        events.push(`createSession:${authId}`);
        return { id: "session-id" };
      },
    },
  };
}

function createOAuthIdentity(): AuthIdentity<"google"> {
  return {
    authId: "auth-id",
    providerName: "google",
    providerUserId: "provider-user-id",
    providerData: JSON.stringify({}),
  };
}
