import type { IncomingMessage, ServerResponse } from "node:http";

import type {
  AuthenticateResult,
  AuthProvider,
  JsonValue,
  ProviderIdentities,
  ServerAdapterFactory,
  WaspServerRuntime,
} from "@wasp.sh/auth-contract";
import { getAuthContractErrorCode } from "@wasp.sh/auth-contract";
import {
  TimeSpan,
  createJWTHelpers,
  hashPassword,
  parseCookies,
  verifyPassword,
} from "@wasp.sh/lib-auth/node";
import { Google, generateCodeVerifier, generateState } from "arctic";

import type { WaspAuthLibOptions } from "./spec.js";
import { WASP_AUTH_PROVIDER_ID } from "./spec.js";

/**
 * Wasp's own auth, lifted out of the compiler's templates into an ordinary
 * auth provider package: the username & password method, the email method
 * (verification links, password reset) and Google OAuth, all running on
 * nothing but the public auth provider contract.
 *
 * Where each in-tree power comes from out here:
 *
 * - Identity storage: `runtime.identityNamespaces(...)`, one namespace per
 *   method (`external:wasp-auth/username`, `/email`, `/google`), password
 *   hashes in the sealed `secrets` channel.
 * - Password hashing and JWTs: `@wasp.sh/lib-auth/node` -- the same
 *   `hashPassword`/`verifyPassword`/`createJWTHelpers` the in-tree flows use,
 *   so stored hashes and token mechanics are format-identical.
 * - Sessions: the `wasp-sessions` grant. Logins mint through
 *   `runtime.sessions.issue` (the session records this provider's id);
 *   password reset revokes through `runtime.sessions.revokeAllForSubject`.
 * - Email: the `email-send` grant -- the app's configured emailSender, with
 *   the manifest-level guarantee that the app HAS one.
 * - OAuth state/PKCE cookies: `runtime.isDevelopment` drives the `secure`
 *   flag, exactly like the in-tree `oauth/cookies.ts`.
 * - The one-time-code handback: this package's own short-lived JWT
 *   (`WASP_AUTH_TOKENS_SECRET`, NOT the framework's reserved `JWT_SECRET`),
 *   redeemed at `POST /wasp-auth/exchange-code` into a minted session the
 *   client adopts via its `setSession` sink.
 */

type Grants = "wasp-sessions" | "identity-namespaces";
type Runtime = WaspServerRuntime<Grants>;

const ID = WASP_AUTH_PROVIDER_ID;
const NS = {
  username: `${ID}/username`,
  email: `${ID}/email`,
  google: `${ID}/google`,
} as const;

export const createServerAdapter: ServerAdapterFactory<
  WaspAuthLibOptions,
  Grants
> = (runtime, options) => {
  const tokens = makeTokenHelpers(runtime);

  const provider: AuthProvider = {
    id: ID,
    // All logins go through this package's own routes; there is no separate
    // credential to exchange, so the exchange route always declines.
    async authenticate(): Promise<AuthenticateResult> {
      return { status: "unauthenticated" };
    },
  };

  return {
    provider,
    routeHandler: makeRouteHandler(runtime, options, tokens),
  };
};

// ---------------------------------------------------------------------------
// Routing
// ---------------------------------------------------------------------------

function makeRouteHandler(
  runtime: Runtime,
  options: WaspAuthLibOptions,
  tokens: TokenHelpers,
) {
  const { methods } = options;

  return async (req: IncomingMessage, res: ServerResponse): Promise<void> => {
    const url = new URL(req.url ?? "/", "http://placeholder");
    const route = `${req.method} ${url.pathname}`;
    const body = (req as IncomingMessage & { body?: unknown }).body ?? {};

    try {
      if (methods.usernameAndPassword !== undefined) {
        switch (route) {
          case "POST /username/signup":
            return await usernameSignup(runtime, body, res);
          case "POST /username/login":
            return await usernameLogin(runtime, body, res);
        }
      }
      if (methods.email !== undefined) {
        switch (route) {
          case "POST /email/signup":
            return await emailSignup(runtime, options, tokens, body, res);
          case "POST /email/verify":
            return await emailVerify(runtime, tokens, body, res);
          case "POST /email/login":
            return await emailLogin(runtime, options, body, res);
          case "POST /email/request-password-reset":
            return await emailRequestPasswordReset(runtime, options, tokens, body, res);
          case "POST /email/reset-password":
            return await emailResetPassword(runtime, tokens, body, res);
        }
      }
      if (methods.google !== undefined) {
        switch (route) {
          case "GET /google/login":
            return await googleLogin(runtime, req, res);
          case "GET /google/callback":
            return await googleCallback(runtime, options, tokens, req, res, url);
        }
      }
      if (route === "POST /exchange-code") {
        return await exchangeOneTimeCode(runtime, tokens, body, res);
      }
      return json(res, 404, { message: "Not found." });
    } catch (error) {
      if (error instanceof ValidationError) {
        return json(res, 400, { message: error.message });
      }
      // The app's onBeforeSignup/onBeforeLogin veto: Wasp tags the thrown
      // error with a contract code, so a policy rejection surfaces as a 4xx
      // with the app's own message, never as a 500.
      if (getAuthContractErrorCode(error) === "wasp-auth/policy-veto") {
        return json(res, 400, {
          message: error instanceof Error ? error.message : "Rejected.",
        });
      }
      console.error("[wasp-auth] request failed:", error);
      return json(res, 500, { message: "Something went wrong." });
    }
  };
}

// ---------------------------------------------------------------------------
// Username & password (the in-tree username method, verbatim in spirit)
// ---------------------------------------------------------------------------

async function usernameSignup(
  runtime: Runtime,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const { username, password } = ensureUsernameArgs(body);
  ensureValidNewPassword(password);

  try {
    await runtime.identityNamespaces(NS.username).create(normalize(username), {
      claims: { username: normalize(username) },
      // Hashing is the flow's explicit job -- storage never hashes.
      secrets: { hashedPassword: await hashPassword(password) },
    });
  } catch (error) {
    if (getAuthContractErrorCode(error) === "wasp-auth/duplicate-identity") {
      return json(res, 422, { message: "Save failed" });
    }
    throw error;
  }
  return json(res, 200, { success: true });
}

async function usernameLogin(
  runtime: Runtime,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const { username, password } = ensureUsernameArgs(body);

  const subjectId = normalize(username);
  const passwordOk = await verifyStoredPassword(
    runtime.identityNamespaces(NS.username),
    subjectId,
    password,
  );
  if (!passwordOk) {
    return json(res, 401, { message: "Invalid credentials" });
  }

  const { sessionId } = await runtime.sessions.issue({
    namespace: NS.username,
    subjectId,
  });
  return json(res, 200, { sessionId });
}

// ---------------------------------------------------------------------------
// Email (verification links, password reset -- the in-tree email method)
// ---------------------------------------------------------------------------

async function emailSignup(
  runtime: Runtime,
  options: WaspAuthLibOptions,
  tokens: TokenHelpers,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const { email, password } = ensureEmailArgs(body);
  ensureValidNewPassword(password);
  const emailConfig = options.methods.email!;
  const subjectId = normalize(email);
  const identities = runtime.identityNamespaces(NS.email);

  const existing = await identities.find(subjectId);
  if (existing !== null) {
    if (existing.data.isEmailVerified === true) {
      // Anti-enumeration, as in-tree: an already-taken address responds
      // exactly like a fresh signup, and no mail is sent.
      return json(res, 200, { success: true });
    }
    // An unverified previous signup gets superseded, as in-tree: whoever
    // proves address ownership first wins.
    await identities.deleteUser(subjectId);
  }

  const skipVerification =
    runtime.isDevelopment && emailConfig.skipEmailVerificationInDev === true;

  await identities.create(subjectId, {
    claims: { email: subjectId },
    data: { isEmailVerified: skipVerification },
    secrets: { hashedPassword: await hashPassword(password) },
  });

  if (!skipVerification) {
    const token = await tokens.create({ kind: "email-verify", subjectId });
    const link = `${runtime.clientUrl}${emailConfig.emailVerificationPath}?token=${token}`;
    await sendEmail(runtime, emailConfig, {
      to: subjectId,
      subject: "Verify your email",
      text: `Click the link below to verify your email: ${link}`,
      html: `<p>Click the link below to verify your email.</p><a href="${link}">Verify email</a>`,
    });
  }
  return json(res, 200, { success: true });
}

async function emailVerify(
  runtime: Runtime,
  tokens: TokenHelpers,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const payload = await tokens.verify(readString(body, "token"), "email-verify");
  if (payload === null) {
    return json(res, 400, { message: "Invalid token" });
  }
  await runtime
    .identityNamespaces(NS.email)
    .updateData(payload.subjectId, { isEmailVerified: true });
  return json(res, 200, { success: true });
}

async function emailLogin(
  runtime: Runtime,
  options: WaspAuthLibOptions,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const { email, password } = ensureEmailArgs(body);
  const subjectId = normalize(email);
  const identities = runtime.identityNamespaces(NS.email);

  const passwordOk = await verifyStoredPassword(identities, subjectId, password);
  if (!passwordOk) {
    return json(res, 401, { message: "Invalid credentials" });
  }

  const identity = await identities.find(subjectId);
  const skipVerification =
    runtime.isDevelopment &&
    options.methods.email!.skipEmailVerificationInDev === true;
  if (identity?.data.isEmailVerified !== true && !skipVerification) {
    return json(res, 401, { message: "Please verify your email first" });
  }

  const { sessionId } = await runtime.sessions.issue({
    namespace: NS.email,
    subjectId,
  });
  return json(res, 200, { sessionId });
}

async function emailRequestPasswordReset(
  runtime: Runtime,
  options: WaspAuthLibOptions,
  tokens: TokenHelpers,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const email = readString(body, "email");
  const subjectId = normalize(email);
  const emailConfig = options.methods.email!;

  const identity = await runtime.identityNamespaces(NS.email).find(subjectId);
  if (identity !== null) {
    const token = await tokens.create({ kind: "password-reset", subjectId });
    const link = `${runtime.clientUrl}${emailConfig.passwordResetPath}?token=${token}`;
    await sendEmail(runtime, emailConfig, {
      to: subjectId,
      subject: "Reset your password",
      text: `Click the link below to reset your password: ${link}`,
      html: `<p>Click the link below to reset your password.</p><a href="${link}">Reset password</a>`,
    });
  }
  // Anti-enumeration: unknown addresses respond exactly like known ones.
  return json(res, 200, { success: true });
}

async function emailResetPassword(
  runtime: Runtime,
  tokens: TokenHelpers,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const payload = await tokens.verify(readString(body, "token"), "password-reset");
  if (payload === null) {
    return json(res, 400, { message: "Invalid token" });
  }
  const password = readString(body, "password");
  ensureValidNewPassword(password);

  const { subjectId } = payload;
  await runtime.identityNamespaces(NS.email).setSecrets(subjectId, {
    hashedPassword: await hashPassword(password),
  });
  // The in-tree reset's session-theft defense: every Wasp session of this
  // user dies, whichever provider minted it.
  await runtime.sessions.revokeAllForSubject({
    namespace: NS.email,
    subjectId,
  });
  return json(res, 200, { success: true });
}

// ---------------------------------------------------------------------------
// Google OAuth (arctic, PKCE, state cookies, one-time-code handback)
// ---------------------------------------------------------------------------

function makeGoogleClient(runtime: Runtime): Google {
  return new Google(
    requireEnv(runtime, "WASP_AUTH_GOOGLE_CLIENT_ID"),
    requireEnv(runtime, "WASP_AUTH_GOOGLE_CLIENT_SECRET"),
    `${runtime.serverUrl}/wasp-auth/google/callback`,
  );
}

async function googleLogin(
  runtime: Runtime,
  req: IncomingMessage,
  res: ServerResponse,
): Promise<void> {
  const state = generateState();
  const codeVerifier = generateCodeVerifier();
  setStateCookies(runtime, res, { state, codeVerifier });

  const url = await makeGoogleClient(runtime).createAuthorizationURL(
    state,
    codeVerifier,
    { scopes: ["profile"] },
  );
  return redirect(res, url.toString());
}

async function googleCallback(
  runtime: Runtime,
  options: WaspAuthLibOptions,
  tokens: TokenHelpers,
  req: IncomingMessage,
  res: ServerResponse,
  url: URL,
): Promise<void> {
  const callbackPath = options.oauthCallbackPath ?? "/oauth/callback";
  try {
    const code = url.searchParams.get("code");
    const state = url.searchParams.get("state");
    const stored = getStateCookies(req);
    if (
      typeof code !== "string" ||
      !state ||
      !stored.state ||
      stored.state !== state ||
      !stored.codeVerifier
    ) {
      throw new Error("Invalid OAuth state");
    }

    const googleTokens = await makeGoogleClient(runtime).validateAuthorizationCode(
      code,
      stored.codeVerifier,
    );
    const profile = await fetchGoogleProfile(googleTokens.accessToken);

    const provisioned = await runtime
      .identityNamespaces(NS.google)
      // The profile is plain parsed JSON; the cast crosses the JsonValue
      // boundary the same way the runtime's own facet wrappers do.
      .provision(profile.sub, {
        claims: profile as unknown as Record<string, JsonValue>,
      });
    if (provisioned === null) {
      throw new Error("Could not provision the user");
    }

    // The handback: a short-lived single-use code in the URL fragment, NOT a
    // session token (fragments land in history; a code is dead 60s later and
    // after first use).
    const oneTimeCode = await tokens.create(
      { kind: "login-code", subjectId: profile.sub, namespace: NS.google },
      new TimeSpan(1, "m"),
    );
    return redirect(res, `${runtime.clientUrl}${callbackPath}#${oneTimeCode}`);
  } catch (error) {
    console.error("[wasp-auth] OAuth callback failed:", error);
    return redirect(res, `${runtime.clientUrl}${callbackPath}?error=oauth-failed`);
  }
}

async function fetchGoogleProfile(
  accessToken: string,
): Promise<{ sub: string } & Record<string, unknown>> {
  const response = await fetch(
    "https://openidconnect.googleapis.com/v1/userinfo",
    { headers: { Authorization: `Bearer ${accessToken}` } },
  );
  const profile = (await response.json()) as { sub?: string };
  if (!profile.sub) {
    throw new Error("Invalid profile");
  }
  return profile as { sub: string } & Record<string, unknown>;
}

async function exchangeOneTimeCode(
  runtime: Runtime,
  tokens: TokenHelpers,
  body: unknown,
  res: ServerResponse,
): Promise<void> {
  const code = readString(body, "code");
  const payload = await tokens.verify(code, "login-code");
  if (payload === null || tokens.isUsed(code)) {
    return json(res, 401, { message: "Invalid code" });
  }
  tokens.markUsed(code);

  const { sessionId } = await runtime.sessions.issue({
    namespace: payload.namespace ?? ID,
    subjectId: payload.subjectId,
  });
  return json(res, 200, { sessionId });
}

// ---------------------------------------------------------------------------
// Tokens (the in-tree jwt.ts + tokenStore, on this package's own secret)
// ---------------------------------------------------------------------------

type TokenKind = "email-verify" | "password-reset" | "login-code";
type TokenPayload = {
  kind: TokenKind;
  subjectId: string;
  namespace?: string;
};
type TokenHelpers = {
  create(payload: TokenPayload, expiresIn?: TimeSpan): Promise<string>;
  verify(token: string, expectedKind: TokenKind): Promise<TokenPayload | null>;
  isUsed(token: string): boolean;
  markUsed(token: string): void;
};

function makeTokenHelpers(runtime: Runtime): TokenHelpers {
  // Delivered through the manifest's env declaration: `devDefault` fills it
  // in development, production requires it -- the same semantics the in-tree
  // JWT_SECRET has, now expressible by any adapter.
  const secret = runtime.env.WASP_AUTH_TOKENS_SECRET;
  if (secret === undefined) {
    throw new Error(
      "WASP_AUTH_TOKENS_SECRET is required: it signs email links and OAuth one-time codes.",
    );
  }
  const { createJWT, validateJWT } = createJWTHelpers(
    new TextEncoder().encode(secret),
    "HS256",
  );

  // In-memory replay protection, same single-instance caveat as the in-tree
  // one-time-code store; the JWT expiry bounds replay on multi-instance
  // deployments.
  const usedTokens = new Map<string, number>();

  return {
    create: (payload, expiresIn = new TimeSpan(30, "m")) =>
      createJWT(payload, { expiresIn }),
    verify: async (token, expectedKind) => {
      try {
        const payload = await validateJWT<TokenPayload>(token);
        return payload.kind === expectedKind ? payload : null;
      } catch {
        return null;
      }
    },
    isUsed: (token) => usedTokens.has(token),
    markUsed: (token) => {
      usedTokens.set(token, Date.now());
      for (const [used, at] of usedTokens) {
        if (Date.now() - at > 1000 * 60 * 60) {
          usedTokens.delete(used);
        }
      }
    },
  };
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

async function verifyStoredPassword(
  identities: ProviderIdentities,
  subjectId: string,
  password: string,
): Promise<boolean> {
  const secrets = await identities.getSecrets(subjectId);
  if (secrets === null || typeof secrets.hashedPassword !== "string") {
    return false;
  }
  try {
    await verifyPassword(secrets.hashedPassword, password);
    return true;
  } catch {
    return false;
  }
}

async function sendEmail(
  runtime: Runtime,
  emailConfig: NonNullable<WaspAuthLibOptions["methods"]["email"]>,
  email: { to: string; subject: string; text: string; html: string },
): Promise<void> {
  // Present iff the manifest requested `email-send`, which the spec helper
  // does exactly when the email method is enabled.
  if (runtime.email === undefined) {
    throw new Error("The email method requires the email-send grant.");
  }
  await runtime.email.send({ from: emailConfig.fromField, ...email });
}

function setStateCookies(
  runtime: Runtime,
  res: ServerResponse,
  values: { state: string; codeVerifier: string },
): void {
  // Same attributes as the in-tree oauth/cookies.ts; `secure` follows the
  // runtime's isDevelopment, the signal the contract gained for this.
  const attributes = [
    "HttpOnly",
    "SameSite=Lax",
    "Path=/",
    "Max-Age=3600",
    ...(runtime.isDevelopment ? [] : ["Secure"]),
  ].join("; ");
  res.setHeader(
    "Set-Cookie",
    Object.entries(values).map(
      ([name, value]) => `wasp_auth_${name}=${value}; ${attributes}`,
    ),
  );
}

function getStateCookies(req: IncomingMessage): {
  state?: string;
  codeVerifier?: string;
} {
  const cookies = parseCookies(req.headers.cookie ?? "");
  return {
    state: cookies.get("wasp_auth_state"),
    codeVerifier: cookies.get("wasp_auth_codeVerifier"),
  };
}

class ValidationError extends Error {}

function requireEnv(runtime: Runtime, name: string): string {
  const value = runtime.env[name];
  if (value === undefined) {
    throw new Error(`${name} is required for the Google method.`);
  }
  return value;
}

function readString(body: unknown, field: string): string {
  const value = ((body ?? {}) as Record<string, unknown>)[field];
  if (typeof value !== "string" || value.length === 0) {
    throw new ValidationError(`${field} must be present`);
  }
  return value;
}

function ensureUsernameArgs(body: unknown): {
  username: string;
  password: string;
} {
  return {
    username: readString(body, "username"),
    password: readString(body, "password"),
  };
}

function ensureEmailArgs(body: unknown): { email: string; password: string } {
  const email = readString(body, "email");
  if (!email.includes("@")) {
    throw new ValidationError("email must be valid");
  }
  return { email, password: readString(body, "password") };
}

// The in-tree rules from wasp/auth/validation.
function ensureValidNewPassword(password: string): void {
  if (password.length < 8) {
    throw new ValidationError("password must be at least 8 characters");
  }
  if (!/\d/.test(password)) {
    throw new ValidationError("password must contain a number");
  }
}

function normalize(value: string): string {
  return value.trim().toLowerCase();
}

function redirect(res: ServerResponse, location: string): void {
  res.statusCode = 302;
  res.setHeader("Location", location);
  res.end();
}

function json(res: ServerResponse, status: number, payload: unknown): void {
  res.statusCode = status;
  res.setHeader("Content-Type", "application/json");
  res.end(JSON.stringify(payload));
}
