import http from 'http';
import express, { Router } from 'express';
import * as z from 'zod';
import { PrismaClient, Prisma } from '@prisma/client';
import { canManageSessions, canRevokeSessions } from '@wasp.sh/auth-contract';
import { Lucia } from 'lucia';
import { PrismaAdapter } from '@lucia-auth/adapter-prisma';
import { validateJWT, createJWT } from 'oslo/jwt';
import { TimeSpan } from 'oslo';
import { hash, verify } from '@node-rs/argon2';
import { parseCookies } from 'oslo/cookie';
import { generateState, generateCodeVerifier, Google } from 'arctic';
import { registerCustom, deserialize, serialize } from 'superjson';
import cookieParser from 'cookie-parser';
import logger from 'morgan';
import cors from 'cors';
import helmet from 'helmet';

function colorize(color, text) {
  if (!supportsAnsiFormatting()) {
    return text;
  }
  const ansiColorCode = ansiColorCodes[color];
  return text.split("\n").map((line) => `${ansiColorCode}${line}${ansiResetCode}`).join("\n");
}
function supportsAnsiFormatting() {
  const isBrowser = !!globalThis.window;
  const isNode = !!globalThis.process;
  if (isBrowser && "chrome" in window) {
    return true;
  }
  if (isNode) {
    if ("NO_COLOR" in process.env) {
      return false;
    }
    return true;
  }
  return false;
}
const ansiColorCodes = {
  red: "\x1B[31m",
  yellow: "\x1B[33m"
};
const ansiResetCode = "\x1B[0m";

function ensureEnvSchema(data, schema) {
  const result = getValidatedEnvOrError(data, schema);
  if (result.success) {
    return result.data;
  } else {
    console.error(colorize("red", formatZodEnvError(result.error)));
    throw new Error("Error parsing environment variables");
  }
}
function getValidatedEnvOrError(env, schema) {
  return schema.safeParse(env);
}
function formatZodEnvError(error) {
  const flattenedIssues = z.flattenError(error);
  return [
    "\u2550\u2550 Env vars validation failed \u2550\u2550",
    "",
    // Top-level errors
    ...flattenedIssues.formErrors,
    "",
    // Errors per field
    ...Object.entries(flattenedIssues.fieldErrors).map(([prop, error2]) => `${prop} - ${error2}`),
    "",
    "\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550"
  ].join("\n");
}

const userServerEnvSchema = z.object({});
const waspCommonServerEnvSchema = z.object({
  PORT: z.coerce.number().default(3001),
  DATABASE_URL: z.string({
    error: "DATABASE_URL is required"
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
  SKIP_EMAIL_VERIFICATION_IN_DEV: z.enum(["true", "false"], {
    error: 'SKIP_EMAIL_VERIFICATION_IN_DEV must be either "true" or "false"'
  }).default("false").transform((value) => value === "true"),
  "WASP_AUTH_GOOGLE_CLIENT_ID": z.string({
    error: "WASP_AUTH_GOOGLE_CLIENT_ID is required by the 'external:wasp-auth' auth provider: Google OAuth client id (framework names like GOOGLE_CLIENT_ID are reserved for Wasp itself)."
  }),
  "WASP_AUTH_GOOGLE_CLIENT_SECRET": z.string({
    error: "WASP_AUTH_GOOGLE_CLIENT_SECRET is required by the 'external:wasp-auth' auth provider: Google OAuth client secret."
  })
});
const serverUrlSchema = z.string({
  error: "WASP_SERVER_URL is required"
}).pipe(z.url({
  error: "WASP_SERVER_URL must be a valid URL"
}));
const clientUrlSchema = z.string({
  error: "WASP_WEB_CLIENT_URL is required"
}).pipe(z.url({
  error: "WASP_WEB_CLIENT_URL must be a valid URL"
}));
const waspDevServerEnvSchema = z.object({
  NODE_ENV: z.literal("development"),
  "WASP_SERVER_URL": serverUrlSchema.default("http://localhost:3001"),
  "WASP_WEB_CLIENT_URL": clientUrlSchema.default("http://localhost:3000/"),
  "WASP_AUTH_TOKENS_SECRET": z.string({
    error: "WASP_AUTH_TOKENS_SECRET is required by the 'external:wasp-auth' auth provider: Signs email verification links, password reset links and OAuth one-time codes. Required in production, defaulted in development."
  }).default("DEV_WASP_AUTH_TOKENS_SECRET")
});
const waspProdServerEnvSchema = z.object({
  NODE_ENV: z.literal("production"),
  "WASP_SERVER_URL": serverUrlSchema,
  "WASP_WEB_CLIENT_URL": clientUrlSchema,
  "WASP_AUTH_TOKENS_SECRET": z.string({
    error: "WASP_AUTH_TOKENS_SECRET is required by the 'external:wasp-auth' auth provider: Signs email verification links, password reset links and OAuth one-time codes. Required in production, defaulted in development."
  })
});
const waspServerEnvSchema = z.discriminatedUnion("NODE_ENV", [
  z.object({ ...waspCommonServerEnvSchema.shape, ...waspDevServerEnvSchema.shape }),
  z.object({ ...waspCommonServerEnvSchema.shape, ...waspProdServerEnvSchema.shape })
]);
const serverEnvSchema = userServerEnvSchema.and(waspServerEnvSchema);
const defaultNodeEnvValue = waspDevServerEnvSchema.shape.NODE_ENV.value;
const { NODE_ENV: inputNodeEnvValue, ...restEnv } = process.env;
const env = ensureEnvSchema({
  NODE_ENV: inputNodeEnvValue ?? defaultNodeEnvValue,
  ...restEnv
}, serverEnvSchema);

function stripTrailingSlash(url) {
  return url?.replace(/\/$/, "");
}
function getOrigin(url) {
  return new URL(url).origin;
}

const frontendUrl = stripTrailingSlash(env["WASP_WEB_CLIENT_URL"]);
const serverUrl = stripTrailingSlash(env["WASP_SERVER_URL"]);
const allowedCORSOriginsPerEnv = {
  development: [/.*/],
  production: [getOrigin(frontendUrl)]
};
const allowedCORSOrigins = allowedCORSOriginsPerEnv[env.NODE_ENV];
const config = {
  frontendUrl,
  serverUrl,
  allowedCORSOrigins,
  env: env.NODE_ENV,
  isDevelopment: env.NODE_ENV === "development",
  port: env.PORT,
  databaseUrl: env.DATABASE_URL
};

function createDbClient() {
  return new PrismaClient({
    // The auth identity's secret material (password hashes, ...) never leaves
    // this column unless auth internals opt back in per query -- so it cannot
    // end up in an operation result or a log by accident.
    omit: {
      authIdentity: {
        providerSecrets: true
      }
    }
  });
}
const dbClient = createDbClient();

class HttpError extends Error {
  statusCode;
  data;
  constructor(statusCode, message, data, options) {
    super(message, options);
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, HttpError);
    }
    this.name = this.constructor.name;
    if (!(Number.isInteger(statusCode) && statusCode >= 400 && statusCode < 600)) {
      throw new Error("statusCode has to be integer in range [400, 600).");
    }
    this.statusCode = statusCode;
    if (data) {
      this.data = data;
    }
  }
}

function normalizeProviderUserId(providerName, providerUserId) {
  switch (providerName) {
    case "email":
    case "username":
      return providerUserId.toLowerCase();
    case "google":
    case "github":
    case "discord":
    case "keycloak":
    case "slack":
    case "microsoft":
      return providerUserId;
    /*
          Why the default case?
          In case users add a new auth provider in the user-land.
          Users can't extend this function because it is private.
          If there is an unknown `providerName` in runtime, we'll
          return the `providerUserId` as is.
    
          We want to still have explicit OAuth providers listed
          so that we get a type error if we forget to add a new provider
          to the switch statement.
        */
    default:
      return providerUserId;
  }
}

function getIdentityStore(providerName) {
  const normalize = (providerUserId) => normalizeProviderUserId(providerName, providerUserId);
  const whereIdentity = (providerUserId) => ({
    providerName_providerUserId: {
      providerName,
      providerUserId: normalize(providerUserId)
    }
  });
  return {
    async find(providerUserId) {
      const identity = await dbClient.authIdentity.findUnique({
        where: whereIdentity(providerUserId)
      });
      if (identity === null) {
        return null;
      }
      return {
        providerName: identity.providerName,
        providerUserId: identity.providerUserId,
        authId: identity.authId,
        data: JSON.parse(identity.providerData),
        claims: JSON.parse(identity.providerClaims)
      };
    },
    async createIdentity(providerUserId, identity, userFields) {
      return dbClient.user.create({
        data: {
          // Using any here to prevent type errors when userFields are not
          // defined. We want Prisma to throw an error in that case.
          ...userFields ?? {},
          auth: {
            create: {
              identities: {
                create: {
                  providerName,
                  providerUserId: normalize(providerUserId),
                  providerClaims: JSON.stringify(identity?.claims ?? {}),
                  providerData: JSON.stringify(identity?.data ?? {}),
                  providerSecrets: JSON.stringify(identity?.secrets ?? {})
                }
              }
            }
          }
        },
        // We need to include the Auth entity here because we need `authId`
        // to be able to create a session.
        include: {
          auth: true
        }
      });
    },
    async provision(providerUserId, identity, userFields) {
      const existing = await this.find(providerUserId);
      if (existing !== null) {
        return { authId: existing.authId };
      }
      try {
        const created = await this.createIdentity(providerUserId, identity, userFields);
        return { authId: created.auth.id };
      } catch (e) {
        if (isUniqueConstraintViolation$2(e)) {
          const raced = await this.find(providerUserId);
          return raced === null ? null : { authId: raced.authId };
        }
        throw e;
      }
    },
    async getSecrets(providerUserId) {
      const identity = await dbClient.authIdentity.findUnique({
        where: whereIdentity(providerUserId),
        omit: { providerSecrets: false }
      });
      return identity === null ? null : JSON.parse(identity.providerSecrets);
    },
    async setSecrets(providerUserId, secrets) {
      await dbClient.authIdentity.update({
        where: whereIdentity(providerUserId),
        data: { providerSecrets: JSON.stringify(secrets) }
      });
    },
    async updateData(providerUserId, updates) {
      const identity = await dbClient.authIdentity.findUnique({
        where: whereIdentity(providerUserId),
        select: { providerData: true }
      });
      if (identity === null) {
        throw new Error("Auth identity not found.");
      }
      const newData = { ...JSON.parse(identity.providerData), ...updates };
      await dbClient.authIdentity.update({
        where: whereIdentity(providerUserId),
        data: { providerData: JSON.stringify(newData) }
      });
    },
    async deleteUser(providerUserId) {
      const { count } = await dbClient.user.deleteMany({
        where: {
          auth: {
            identities: {
              some: {
                providerName,
                providerUserId: normalize(providerUserId)
              }
            }
          }
        }
      });
      return count > 0;
    }
  };
}
function isUniqueConstraintViolation$2(e) {
  return typeof e === "object" && e !== null && "code" in e && e.code === "P2002";
}

const prismaAdapter = new PrismaAdapter(dbClient.session, dbClient.auth);
const auth$1 = new Lucia(prismaAdapter, {
  // Since we are not using cookies, we don't need to set any cookie options.
  // But in the future, if we decide to use cookies, we can set them here.
  // sessionCookie: {
  //   name: "session",
  //   expires: true,
  //   attributes: {
  //     secure: !config.isDevelopment,
  //     sameSite: "lax",
  //   },
  // },
  getSessionAttributes({ providerId, providerSessionId }) {
    return {
      providerId,
      providerSessionId
    };
  },
  getUserAttributes({ userId }) {
    return {
      userId
    };
  }
});

function getBearerToken(header) {
  const prefix = "Bearer ";
  if (typeof header !== "string" || !header.startsWith(prefix)) {
    return null;
  }
  return header.substring(prefix.length);
}
async function createSession(authId, options) {
  const session = await auth$1.createSession(authId, {
    providerId: options.providerId,
    providerSessionId: options.providerSessionId ?? null
  });
  return { id: session.id };
}
async function validateSession(token) {
  const { session } = await auth$1.validateSession(token);
  if (!session) {
    return null;
  }
  if (session.providerId === null) {
    await auth$1.invalidateSession(session.id);
    return null;
  }
  return {
    id: session.id,
    authId: session.userId,
    providerId: session.providerId,
    providerSessionId: session.providerSessionId
  };
}
async function getStoredSession(sessionId) {
  const session = await dbClient.session.findUnique({
    where: { id: sessionId },
    select: { id: true, userId: true, providerId: true, providerSessionId: true }
  });
  if (!session) {
    return null;
  }
  return { id: session.id, authId: session.userId, providerId: session.providerId, providerSessionId: session.providerSessionId };
}
function revokeSession(sessionId) {
  return auth$1.invalidateSession(sessionId);
}
function revokeAllSessions(authId) {
  return auth$1.invalidateUserSessions(authId);
}

const defineHandler = (middleware) => middleware;

function throwValidationError(message) {
  throw new HttpError(422, "Validation failed", { message });
}

({
  entities: {
    User: dbClient.user
  }
});
async function findAuthWithUserBy(where) {
  const result = await dbClient.auth.findFirst({ where, include: { user: true } });
  if (result === null) {
    return null;
  }
  if (result.user === null) {
    return null;
  }
  return { ...result, user: result.user };
}
async function validateAndGetUserFields(data, userSignupFields) {
  const { password: _password, ...sanitizedData } = data;
  const result = {};
  if (!userSignupFields) {
    return result;
  }
  for (const [field, getFieldValue] of Object.entries(userSignupFields)) {
    try {
      const value = await getFieldValue(sanitizedData);
      result[field] = value;
    } catch (e) {
      throwValidationError(e.message);
    }
  }
  return result;
}
function createInvalidCredentialsError(message) {
  return new HttpError(401, "Invalid credentials", { message });
}

const onBeforeSignup = async ({ providerId }) => {
  if (providerId.providerUserId.includes("blocked")) {
    throw new Error("This name is not allowed.");
  }
  console.log(
    `[hooks] onBeforeSignup: ${providerId.providerName}/${providerId.providerUserId}`
  );
};
const onAfterLogin = async ({ providerId, user }) => {
  console.log(
    `[hooks] onAfterLogin: ${providerId.providerName}/${providerId.providerUserId} -> user ${user.id}`
  );
};

async function fireVetoableHook(fire) {
  try {
    await fire();
  } catch (error) {
    if (typeof error === "object" && error !== null && !error.code) {
      try {
        ;
        error.code = "wasp-auth/policy-veto";
      } catch {
      }
    }
    throw error;
  }
}
const onBeforeSignupHook = (params) => onBeforeSignup({ ...params });
const onAfterSignupHook = async (_params) => {
};
const onBeforeLoginHook = async (_params) => {
};
const onAfterLoginHook = (params) => onAfterLogin({ ...params });

function getDefaultFromField() {
  return {
    email: "auth@example.com",
    name: "Wasp Auth Lib"
  };
}

function initDummyEmailSender(_config) {
  const defaultFromField = getDefaultFromField();
  return {
    send: async (email) => {
      const fromField = email.from || defaultFromField;
      console.log(colorize("yellow", "\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557"));
      console.log(colorize("yellow", "\u2551 Dummy email sender \u2709\uFE0F  \u2551"));
      console.log(colorize("yellow", "\u255A\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255D"));
      console.log(`From:    ${fromField.name} <${fromField.email}>`);
      console.log(`To:      ${email.to}`);
      console.log(`Subject: ${email.subject}`);
      console.log(colorize("yellow", "\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550 Text \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550"));
      console.log(email.text);
      console.log(colorize("yellow", "\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550 HTML \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550"));
      console.log(email.html);
      console.log(colorize("yellow", "\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550"));
      return {
        success: true
      };
    }
  };
}

const emailSender = initDummyEmailSender();

function getAuthContractErrorCode(error) {
  if (typeof error !== "object" || error === null || !("code" in error)) return null;
  const code = error.code;
  return code === "wasp-auth/duplicate-identity" || code === "wasp-auth/identity-not-found" || code === "wasp-auth/undeclared-namespace" || code === "wasp-auth/policy-veto" ? code : null;
}

function createJWTHelpers(JWT_SECRET, JWT_ALGORITHM) {
  return {
    /**
    * Creates a JWT token with the given payload and options.
    * @param data The payload to include in the JWT token.
    * @param options Additional options for `oslo/jwt`'s `createJWT` function.
    * @returns The created JWT token.
    */
    createJWT: (data, options) => {
      return createJWT(JWT_ALGORITHM, JWT_SECRET, data, options);
    },
    /**
    * Validates a JWT token and returns its payload if valid.
    * @param token
    * @returns The payload of the JWT token.
    * @throws If the token is invalid or expired.
    */
    validateJWT: async (token) => {
      const { payload } = await validateJWT(JWT_ALGORITHM, JWT_SECRET, token);
      return payload;
    }
  };
}
const hashingOptions = {
  memoryCost: 19456,
  timeCost: 2,
  outputLen: 32,
  parallelism: 1
};
async function hashPassword(password) {
  return hash(normalizePassword(password), hashingOptions);
}
async function verifyPassword(hashedPassword, password) {
  if (!await verify(hashedPassword, normalizePassword(password), hashingOptions)) throw new Error("Invalid password");
}
function normalizePassword(password) {
  return password.normalize("NFKC");
}

const WASP_AUTH_PROVIDER_ID = "external:wasp-auth";

const ID = WASP_AUTH_PROVIDER_ID;
const NS = {
  username: `${ID}/username`,
  email: `${ID}/email`,
  google: `${ID}/google`
};
const createServerAdapter = (runtime, options) => {
  const tokens = makeTokenHelpers(runtime);
  const provider = {
    id: ID,
    // All logins go through this package's own routes; there is no separate
    // credential to exchange, so the exchange route always declines.
    async authenticate() {
      return { status: "unauthenticated" };
    }
  };
  return {
    provider,
    routeHandler: makeRouteHandler(runtime, options, tokens)
  };
};
function makeRouteHandler(runtime, options, tokens) {
  const { methods } = options;
  return async (req, res) => {
    const url = new URL(req.url ?? "/", "http://placeholder");
    const route = `${req.method} ${url.pathname}`;
    const body = req.body ?? {};
    try {
      if (methods.usernameAndPassword !== void 0) {
        switch (route) {
          case "POST /username/signup":
            return await usernameSignup(runtime, body, res);
          case "POST /username/login":
            return await usernameLogin(runtime, body, res);
        }
      }
      if (methods.email !== void 0) {
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
      if (methods.google !== void 0) {
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
      if (getAuthContractErrorCode(error) === "wasp-auth/policy-veto") {
        return json(res, 400, {
          message: error instanceof Error ? error.message : "Rejected."
        });
      }
      console.error("[wasp-auth] request failed:", error);
      return json(res, 500, { message: "Something went wrong." });
    }
  };
}
async function usernameSignup(runtime, body, res) {
  const { username, password } = ensureUsernameArgs(body);
  ensureValidNewPassword(password);
  try {
    await runtime.identityNamespaces(NS.username).create(normalize(username), {
      claims: { username: normalize(username) },
      // Hashing is the flow's explicit job -- storage never hashes.
      secrets: { hashedPassword: await hashPassword(password) }
    });
  } catch (error) {
    if (getAuthContractErrorCode(error) === "wasp-auth/duplicate-identity") {
      return json(res, 422, { message: "Save failed" });
    }
    throw error;
  }
  return json(res, 200, { success: true });
}
async function usernameLogin(runtime, body, res) {
  const { username, password } = ensureUsernameArgs(body);
  const subjectId = normalize(username);
  const passwordOk = await verifyStoredPassword(runtime.identityNamespaces(NS.username), subjectId, password);
  if (!passwordOk) {
    return json(res, 401, { message: "Invalid credentials" });
  }
  const { sessionId } = await runtime.sessions.issue({
    namespace: NS.username,
    subjectId
  });
  return json(res, 200, { sessionId });
}
async function emailSignup(runtime, options, tokens, body, res) {
  const { email, password } = ensureEmailArgs(body);
  ensureValidNewPassword(password);
  const emailConfig = options.methods.email;
  const subjectId = normalize(email);
  const identities = runtime.identityNamespaces(NS.email);
  const existing = await identities.find(subjectId);
  if (existing !== null) {
    if (existing.data.isEmailVerified === true) {
      return json(res, 200, { success: true });
    }
    await identities.deleteUser(subjectId);
  }
  const skipVerification = runtime.isDevelopment && emailConfig.skipEmailVerificationInDev === true;
  await identities.create(subjectId, {
    claims: { email: subjectId },
    data: { isEmailVerified: skipVerification },
    secrets: { hashedPassword: await hashPassword(password) }
  });
  if (!skipVerification) {
    const token = await tokens.create({ kind: "email-verify", subjectId });
    const link = `${runtime.clientUrl}${emailConfig.emailVerificationPath}?token=${token}`;
    await sendEmail(runtime, emailConfig, {
      to: subjectId,
      subject: "Verify your email",
      text: `Click the link below to verify your email: ${link}`,
      html: `<p>Click the link below to verify your email.</p><a href="${link}">Verify email</a>`
    });
  }
  return json(res, 200, { success: true });
}
async function emailVerify(runtime, tokens, body, res) {
  const payload = await tokens.verify(readString(body, "token"), "email-verify");
  if (payload === null) {
    return json(res, 400, { message: "Invalid token" });
  }
  await runtime.identityNamespaces(NS.email).updateData(payload.subjectId, { isEmailVerified: true });
  return json(res, 200, { success: true });
}
async function emailLogin(runtime, options, body, res) {
  const { email, password } = ensureEmailArgs(body);
  const subjectId = normalize(email);
  const identities = runtime.identityNamespaces(NS.email);
  const passwordOk = await verifyStoredPassword(identities, subjectId, password);
  if (!passwordOk) {
    return json(res, 401, { message: "Invalid credentials" });
  }
  const identity = await identities.find(subjectId);
  const skipVerification = runtime.isDevelopment && options.methods.email.skipEmailVerificationInDev === true;
  if (identity?.data.isEmailVerified !== true && !skipVerification) {
    return json(res, 401, { message: "Please verify your email first" });
  }
  const { sessionId } = await runtime.sessions.issue({
    namespace: NS.email,
    subjectId
  });
  return json(res, 200, { sessionId });
}
async function emailRequestPasswordReset(runtime, options, tokens, body, res) {
  const email = readString(body, "email");
  const subjectId = normalize(email);
  const emailConfig = options.methods.email;
  const identity = await runtime.identityNamespaces(NS.email).find(subjectId);
  if (identity !== null) {
    const token = await tokens.create({ kind: "password-reset", subjectId });
    const link = `${runtime.clientUrl}${emailConfig.passwordResetPath}?token=${token}`;
    await sendEmail(runtime, emailConfig, {
      to: subjectId,
      subject: "Reset your password",
      text: `Click the link below to reset your password: ${link}`,
      html: `<p>Click the link below to reset your password.</p><a href="${link}">Reset password</a>`
    });
  }
  return json(res, 200, { success: true });
}
async function emailResetPassword(runtime, tokens, body, res) {
  const payload = await tokens.verify(readString(body, "token"), "password-reset");
  if (payload === null) {
    return json(res, 400, { message: "Invalid token" });
  }
  const password = readString(body, "password");
  ensureValidNewPassword(password);
  const { subjectId } = payload;
  await runtime.identityNamespaces(NS.email).setSecrets(subjectId, {
    hashedPassword: await hashPassword(password)
  });
  await runtime.sessions.revokeAllForSubject({
    namespace: NS.email,
    subjectId
  });
  return json(res, 200, { success: true });
}
function makeGoogleClient(runtime) {
  return new Google(requireEnv(runtime, "WASP_AUTH_GOOGLE_CLIENT_ID"), requireEnv(runtime, "WASP_AUTH_GOOGLE_CLIENT_SECRET"), `${runtime.serverUrl}/wasp-auth/google/callback`);
}
async function googleLogin(runtime, req, res) {
  const state = generateState();
  const codeVerifier = generateCodeVerifier();
  setStateCookies(runtime, res, { state, codeVerifier });
  const url = await makeGoogleClient(runtime).createAuthorizationURL(state, codeVerifier, { scopes: ["profile"] });
  return redirect(res, url.toString());
}
async function googleCallback(runtime, options, tokens, req, res, url) {
  const callbackPath = options.oauthCallbackPath ?? "/oauth/callback";
  try {
    const code = url.searchParams.get("code");
    const state = url.searchParams.get("state");
    const stored = getStateCookies(req);
    if (typeof code !== "string" || !state || !stored.state || stored.state !== state || !stored.codeVerifier) {
      throw new Error("Invalid OAuth state");
    }
    const googleTokens = await makeGoogleClient(runtime).validateAuthorizationCode(code, stored.codeVerifier);
    const profile = await fetchGoogleProfile(googleTokens.accessToken);
    const provisioned = await runtime.identityNamespaces(NS.google).provision(profile.sub, {
      claims: profile
    });
    if (provisioned === null) {
      throw new Error("Could not provision the user");
    }
    const oneTimeCode = await tokens.create({ kind: "login-code", subjectId: profile.sub, namespace: NS.google }, new TimeSpan(1, "m"));
    return redirect(res, `${runtime.clientUrl}${callbackPath}#${oneTimeCode}`);
  } catch (error) {
    console.error("[wasp-auth] OAuth callback failed:", error);
    return redirect(res, `${runtime.clientUrl}${callbackPath}?error=oauth-failed`);
  }
}
async function fetchGoogleProfile(accessToken) {
  const response = await fetch("https://openidconnect.googleapis.com/v1/userinfo", { headers: { Authorization: `Bearer ${accessToken}` } });
  const profile = await response.json();
  if (!profile.sub) {
    throw new Error("Invalid profile");
  }
  return profile;
}
async function exchangeOneTimeCode(runtime, tokens, body, res) {
  const code = readString(body, "code");
  const payload = await tokens.verify(code, "login-code");
  if (payload === null || tokens.isUsed(code)) {
    return json(res, 401, { message: "Invalid code" });
  }
  tokens.markUsed(code);
  const { sessionId } = await runtime.sessions.issue({
    namespace: payload.namespace ?? ID,
    subjectId: payload.subjectId
  });
  return json(res, 200, { sessionId });
}
function makeTokenHelpers(runtime) {
  const secret = runtime.env.WASP_AUTH_TOKENS_SECRET;
  if (secret === void 0) {
    throw new Error("WASP_AUTH_TOKENS_SECRET is required: it signs email links and OAuth one-time codes.");
  }
  const { createJWT, validateJWT } = createJWTHelpers(new TextEncoder().encode(secret), "HS256");
  const usedTokens = /* @__PURE__ */ new Map();
  return {
    create: (payload, expiresIn = new TimeSpan(30, "m")) => createJWT(payload, { expiresIn }),
    verify: async (token, expectedKind) => {
      try {
        const payload = await validateJWT(token);
        return payload.kind === expectedKind ? payload : null;
      } catch {
        return null;
      }
    },
    isUsed: (token) => usedTokens.has(token),
    markUsed: (token) => {
      usedTokens.set(token, Date.now());
      for (const [used, at] of usedTokens) {
        if (Date.now() - at > 1e3 * 60 * 60) {
          usedTokens.delete(used);
        }
      }
    }
  };
}
async function verifyStoredPassword(identities, subjectId, password) {
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
async function sendEmail(runtime, emailConfig, email) {
  if (runtime.email === void 0) {
    throw new Error("The email method requires the email-send grant.");
  }
  await runtime.email.send({ from: emailConfig.fromField, ...email });
}
function setStateCookies(runtime, res, values) {
  const attributes = [
    "HttpOnly",
    "SameSite=Lax",
    "Path=/",
    "Max-Age=3600",
    ...runtime.isDevelopment ? [] : ["Secure"]
  ].join("; ");
  res.setHeader("Set-Cookie", Object.entries(values).map(([name, value]) => `wasp_auth_${name}=${value}; ${attributes}`));
}
function getStateCookies(req) {
  const cookies = parseCookies(req.headers.cookie ?? "");
  return {
    state: cookies.get("wasp_auth_state"),
    codeVerifier: cookies.get("wasp_auth_codeVerifier")
  };
}
class ValidationError extends Error {
}
function requireEnv(runtime, name) {
  const value = runtime.env[name];
  if (value === void 0) {
    throw new Error(`${name} is required for the Google method.`);
  }
  return value;
}
function readString(body, field) {
  const value = (body ?? {})[field];
  if (typeof value !== "string" || value.length === 0) {
    throw new ValidationError(`${field} must be present`);
  }
  return value;
}
function ensureUsernameArgs(body) {
  return {
    username: readString(body, "username"),
    password: readString(body, "password")
  };
}
function ensureEmailArgs(body) {
  const email = readString(body, "email");
  if (!email.includes("@")) {
    throw new ValidationError("email must be valid");
  }
  return { email, password: readString(body, "password") };
}
function ensureValidNewPassword(password) {
  if (password.length < 8) {
    throw new ValidationError("password must be at least 8 characters");
  }
  if (!/\d/.test(password)) {
    throw new ValidationError("password must contain a number");
  }
}
function normalize(value) {
  return value.trim().toLowerCase();
}
function redirect(res, location) {
  res.statusCode = 302;
  res.setHeader("Location", location);
  res.end();
}
function json(res, status, payload) {
  res.statusCode = status;
  res.setHeader("Content-Type", "application/json");
  res.end(JSON.stringify(payload));
}

function contractError(code, message) {
  const error = new Error(message);
  error.code = code;
  return error;
}
function isUniqueConstraintViolation$1(e) {
  return typeof e === "object" && e !== null && "code" in e && e.code === "P2002";
}
function resolveOwnNamespace(spec, namespace) {
  const resolved = namespace ?? spec.providerId;
  if (!spec.identityNamespaces.includes(resolved)) {
    throw contractError("wasp-auth/undeclared-namespace", `Auth provider '${spec.providerId}' tried to use the identity namespace '${resolved}', which its manifest does not declare.`);
  }
  return resolved;
}
function makeIdentitiesFacet(spec, namespace) {
  const store = getIdentityStore(namespace);
  return {
    find: (subjectId) => store.find(subjectId),
    provision: (subjectId, identity) => provisionAuthUser(spec.providerId, subjectId, identity?.claims, {
      data: identity?.data,
      secrets: identity?.secrets
    }, namespace),
    create: async (subjectId, identity, getUserFields, opts) => {
      if (opts?.skipHooks !== true) {
        await fireVetoableHook(() => onBeforeSignupHook({
          req: opts?.req,
          providerId: makeHookProviderId$1(namespace, subjectId)
        }));
      }
      const userFields = getUserFields !== void 0 ? await getUserFields() : await computeProviderUserFields(spec.providerId, identity?.claims);
      let created;
      try {
        created = await store.createIdentity(subjectId, identity, userFields);
      } catch (e) {
        if (isUniqueConstraintViolation$1(e)) {
          throw contractError("wasp-auth/duplicate-identity", `An identity for this subject already exists in namespace '${namespace}'.`);
        }
        throw e;
      }
      if (opts?.skipHooks !== true) {
        await onAfterSignupHook({
          req: opts?.req,
          oauth: opts?.hookContext
        });
      }
      return { authId: created.auth.id };
    },
    updateData: (subjectId, updates) => store.updateData(subjectId, updates),
    getSecrets: (subjectId) => store.getSecrets(subjectId),
    setSecrets: (subjectId, secrets) => store.setSecrets(subjectId, secrets),
    deleteUser: (subjectId) => store.deleteUser(subjectId)
  };
}
function makeSessionsFacet(spec) {
  const resolveSubjectAuthId = async (subject) => {
    const namespace = resolveOwnNamespace(spec, subject.namespace);
    const identity = await getIdentityStore(namespace).find(subject.subjectId);
    if (identity === null) {
      throw contractError("wasp-auth/identity-not-found", `No identity for the subject in namespace '${namespace}'. Provision it before minting or revoking sessions.`);
    }
    return identity.authId;
  };
  return {
    issue: async (subject, opts) => {
      const authId = await resolveSubjectAuthId(subject);
      const fireHooks = opts?.skipHooks !== true;
      const hookProviderId = makeHookProviderId$1(resolveOwnNamespace(spec, subject.namespace), subject.subjectId);
      let hookUser = void 0;
      if (fireHooks) {
        const auth = await findAuthWithUserBy({ id: authId });
        if (auth === null) {
          throw contractError("wasp-auth/identity-not-found", "The subject resolves to an auth entity with no user.");
        }
        hookUser = auth.user;
        await fireVetoableHook(() => onBeforeLoginHook({
          req: opts?.req,
          providerId: hookProviderId,
          user: auth.user
        }));
      }
      const session = await createSession(authId, {
        providerId: spec.providerId,
        providerSessionId: opts?.providerSessionId
      });
      if (fireHooks) {
        await onAfterLoginHook({
          req: opts?.req,
          providerId: hookProviderId,
          user: hookUser,
          oauth: opts?.hookContext
        });
      }
      return { sessionId: session.id };
    },
    revoke: (sessionId) => revokeSession(sessionId),
    revokeAllForSubject: async (subject) => {
      const authId = await resolveSubjectAuthId(subject);
      await revokeAllSessions(authId);
    }
  };
}
function makeHookProviderId$1(namespace, subjectId) {
  return { providerName: namespace, providerUserId: subjectId };
}
const waspEmailFacet = (() => {
  const configured = { "email": "auth@example.com", "name": "Wasp Auth Lib" };
  const defaultFrom = configured === void 0 ? void 0 : { email: configured.email, ...{ name: configured.name }  };
  return {
    defaultFrom,
    send: async (email) => {
      const from = email.from ?? defaultFrom;
      if (from === void 0) {
        throw new Error("Sending an email through the auth provider runtime requires a `from` field, because the app declares no emailSender.defaultFrom.");
      }
      await emailSender.send({
        from,
        to: email.to,
        subject: email.subject,
        text: email.text,
        html: email.html
      });
    }
  };
})();
function makeAdapterRuntime(spec) {
  return {
    db: dbClient,
    dbProvider: "sqlite",
    // Exactly the vars the manifest declared -- read from the VALIDATED env,
    // so `devDefault`s apply -- and framework secrets (JWT_SECRET) stay
    // unreachable (declaring a framework-owned name is a compile error).
    env: Object.fromEntries(spec.serverEnvVarNames.map((name) => [
      name,
      env[name]
    ])),
    serverUrl: config.serverUrl,
    clientUrl: config.frontendUrl,
    isDevelopment: config.isDevelopment,
    identities: makeIdentitiesFacet(spec, spec.providerId),
    // Granted facets: wired only when the manifest requested them, so an
    // undeclared access fails loudly at first use rather than working by
    // accident.
    ...spec.uses.includes("wasp-sessions") ? { sessions: makeSessionsFacet(spec) } : {},
    ...spec.uses.includes("email-send") ? { email: waspEmailFacet } : {},
    ...spec.uses.includes("identity-namespaces") ? {
      identityNamespaces: (namespace) => makeIdentitiesFacet(spec, resolveOwnNamespace(spec, namespace))
    } : {}
  };
}
const serverAdapter_0 = await Promise.resolve(createServerAdapter(
  // The cast narrows the built runtime to the grants the factory's type
  // declares; the generator wired exactly the manifest's `uses`, and the
  // boot assert keeps manifest and adapter honest.
  makeAdapterRuntime({
    providerId: "external:wasp-auth",
    serverEnvVarNames: ["WASP_AUTH_TOKENS_SECRET", "WASP_AUTH_GOOGLE_CLIENT_ID", "WASP_AUTH_GOOGLE_CLIENT_SECRET"],
    uses: ["wasp-sessions", "identity-namespaces", "email-send"],
    identityNamespaces: ["external:wasp-auth", "external:wasp-auth/username", "external:wasp-auth/email", "external:wasp-auth/google"]
  }),
  { "methods": { "usernameAndPassword": {}, "email": { "emailVerificationPath": "/email-verified", "passwordResetPath": "/password-reset" }, "google": {} }, "oauthCallbackPath": "/oauth/callback" }));
const authProviders = {
  "external:wasp-auth": serverAdapter_0.provider
};
const externalAuthProviders = {
  "external:wasp-auth": authProviders["external:wasp-auth"]
};
function getAuthProvider(providerId) {
  return authProviders[providerId];
}
const authProviderRouteHandlers = {
  "external:wasp-auth": serverAdapter_0.routeHandler
};
function assertProvidersMatchManifests() {
  const manifests = [
    { providerId: "external:wasp-auth", capabilities: [], uses: ["wasp-sessions", "identity-namespaces", "email-send"] }
  ];
  const knownRuntimeGrants = ["wasp-sessions", "email-send", "identity-namespaces"];
  const errors = [];
  for (const manifest of manifests) {
    const provider = getAuthProvider(manifest.providerId);
    if (provider === void 0) {
      continue;
    }
    if (!manifest.providerId.startsWith("external:")) {
      errors.push(`the manifest declares id '${manifest.providerId}', which does not start with 'external:' -- the unprefixed namespace is reserved for Wasp's own auth methods`);
    }
    if (manifest.capabilities.includes("cookie-transport") && !manifest.capabilities.includes("session-revocation")) {
      errors.push(`the manifest for '${manifest.providerId}' declares 'cookie-transport' without 'session-revocation' -- a cookie-borne credential Wasp cannot revoke server-side would make logout() a lie`);
    }
    for (const grant of manifest.uses) {
      if (!knownRuntimeGrants.includes(grant)) {
        errors.push(`the manifest for '${manifest.providerId}' requests the unknown runtime grant '${grant}' -- the generator could not have wired it`);
      }
    }
    if (provider.id !== manifest.providerId) {
      errors.push(`the manifest declares id '${manifest.providerId}', but the adapter's id is '${provider.id}' -- identities are recorded under the provider id, so the two must match`);
    }
    if (manifest.capabilities.includes("issue-sessions") && !canManageSessions(provider)) {
      errors.push(`the manifest for '${manifest.providerId}' declares the 'issue-sessions' capability, but the adapter does not implement the full issueSession/revokeSession/revokeAllSessions set Wasp requires for session management`);
    }
    if (manifest.capabilities.includes("session-revocation") && !canRevokeSessions(provider)) {
      errors.push(`the manifest for '${manifest.providerId}' declares the 'session-revocation' capability, but the adapter does not implement revokeSession`);
    }
  }
  if (errors.length > 0) {
    throw new Error("Auth provider adapters do not match their manifests:\n" + errors.map((error) => `  - ${error}`).join("\n"));
  }
}
assertProvidersMatchManifests();

function makeAuthUserIfPossible(user) {
  return user ? makeAuthUser(user) : null;
}
function makeAuthUser(data) {
  return {
    ...data,
    // The identities map only carries Wasp's own auth methods, and none are
    // enabled without waspAuth among the providers, so there is nothing to
    // read. External identities are reachable server-side through the
    // identity store.
    getFirstProviderUserId: () => null
  };
}
function createAuthUserData(user, sessionProviderId) {
  const { auth, ...rest } = user;
  if (!auth) {
    throw new Error(`\u{1F41D} Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`);
  }
  const identities = {};
  return {
    ...rest,
    sessionProviderId,
    identities
  };
}

async function getSessionAndUserFromBearerToken(req) {
  const token = getBearerToken(req.headers.authorization);
  return token === null ? null : getSessionAndUserFromSessionId(token);
}
async function getSessionAndUserFromSessionId(sessionId) {
  const session = await validateSession(sessionId);
  if (session === null) {
    return null;
  }
  return loadSessionAndUser(session.id, session.authId, session.providerId);
}
async function loadSessionAndUser(sessionId, authId, sessionProviderId) {
  const user = await dbClient.user.findFirst({
    where: { auth: { id: authId } },
    include: {
      auth: {
        include: {
          identities: true
        }
      }
    }
  });
  if (!user) {
    return null;
  }
  return { sessionId, user: createAuthUserData(user, sessionProviderId) };
}
const userSignupFieldsByProviderId = {
  "external:wasp-auth": void 0
};
async function exchangeRequestForSession(providerId, req) {
  const provider = externalAuthProviders[providerId];
  if (provider === void 0) {
    return null;
  }
  const result = await Promise.resolve().then(() => provider.authenticate(toWebRequest(req))).catch((error) => {
    console.error(`Auth provider '${providerId}' threw while authenticating:`, error);
    return { status: "unauthenticated" };
  });
  if (result.status !== "authenticated") {
    return null;
  }
  const { sessionId: providerSessionId, subjectId, claims } = result.session;
  const authId = await resolveExternalSubject(providerId, subjectId, claims, void 0, void 0, req);
  if (authId === null) {
    return null;
  }
  const auth = await findAuthWithUserBy({ id: authId });
  if (auth === null) {
    return null;
  }
  await fireVetoableHook(() => onBeforeLoginHook({
    req,
    providerId: makeHookProviderId(providerId, subjectId),
    user: auth.user
  }));
  const session = await createSession(authId, {
    providerId,
    providerSessionId
  });
  await onAfterLoginHook({
    req,
    providerId: makeHookProviderId(providerId, subjectId),
    user: auth.user
  });
  return session;
}
function toWebRequest(req) {
  const headers = new Headers();
  for (const [key, value] of Object.entries(req.headers)) {
    if (typeof value === "string") {
      headers.set(key, value);
    } else if (Array.isArray(value)) {
      headers.set(key, value.join(", "));
    }
  }
  const host = req.get("host") ?? "localhost";
  return new Request(`${req.protocol}://${host}${req.originalUrl}`, {
    method: req.method,
    headers
  });
}
async function resolveExternalSubject(providerId, subjectId, claims, identity, namespace = providerId, req) {
  const identities = getIdentityStore(namespace);
  const existing = await identities.find(subjectId);
  if (existing) {
    return existing.authId;
  }
  await fireVetoableHook(() => onBeforeSignupHook({
    req,
    providerId: makeHookProviderId(namespace, subjectId)
  }));
  const userFields = await computeProviderUserFields(providerId, claims);
  let created;
  try {
    created = await identities.createIdentity(
      subjectId,
      {
        // The provider-verified profile data (email, name, ...) as of the
        // moment this subject was first seen. Wasp-written and read-only
        // afterwards, so its provenance can be trusted.
        claims: { ...claims ?? {} },
        data: identity?.data,
        secrets: identity?.secrets
      },
      // Using `any` to defer validation of required-but-unset fields to
      // Prisma, which reports them precisely.
      userFields
    );
  } catch (e) {
    if (isUniqueConstraintViolation(e)) {
      const raced = await identities.find(subjectId);
      return raced === null ? null : raced.authId;
    }
    throw e;
  }
  await onAfterSignupHook();
  return created.auth.id;
}
function isUniqueConstraintViolation(e) {
  return typeof e === "object" && e !== null && "code" in e && e.code === "P2002";
}
function makeHookProviderId(namespace, subjectId) {
  return { providerName: namespace, providerUserId: subjectId };
}
async function provisionAuthUser(providerId, subjectId, claims, identity, namespace) {
  const authId = await resolveExternalSubject(providerId, subjectId, claims, identity, namespace);
  return authId === null ? null : { authId };
}
async function computeProviderUserFields(providerId, claims) {
  return validateAndGetUserFields({ ...claims ?? {} }, userSignupFieldsByProviderId[providerId]);
}
async function invalidateSession(sessionId) {
  const stored = await getStoredSession(sessionId);
  await revokeSession(sessionId);
  if (stored?.providerSessionId != null && stored.providerId != null) {
    const provider = getAuthProvider(stored.providerId);
    if (provider !== void 0 && canRevokeSessions(provider)) {
      try {
        await provider.revokeSession(stored.providerSessionId);
      } catch (error) {
        console.error("Wasp session revoked, but revoking the auth provider session failed:", error);
      }
    }
  }
}

const auth = defineHandler(async (req, res, next) => {
  const authHeader = req.get("Authorization");
  if (!authHeader) {
    req.sessionId = null;
    req.user = null;
    return next();
  }
  const sessionAndUser = await getSessionAndUserFromBearerToken(req);
  if (sessionAndUser === null) {
    throw createInvalidCredentialsError();
  }
  req.sessionId = sessionAndUser.sessionId;
  req.user = sessionAndUser.user;
  next();
});

const Decimal = Prisma.Decimal;
if (Decimal) {
  registerCustom({
    isApplicable: (v) => Decimal.isDecimal(v),
    serialize: (v) => v.toJSON(),
    deserialize: (v) => new Decimal(v)
  }, "prisma.decimal");
}

function createOperation(handlerFn) {
  return defineHandler(async (req, res) => {
    const args = req.body && deserialize(req.body) || {};
    const context = {
      user: makeAuthUserIfPossible(req.user)
    };
    const result = await handlerFn(args, context);
    const serializedResult = serialize(result);
    res.json(serializedResult);
  });
}
function createQuery(handlerFn) {
  return createOperation(handlerFn);
}
function createAction(handlerFn) {
  return createOperation(handlerFn);
}

const getMyTasks$2 = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.findMany({
    where: { userId: context.user.id },
    orderBy: { id: "asc" }
  });
};
const createTask$2 = async ({ description }, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.create({
    data: { description, userId: context.user.id }
  });
};

async function createTask$1(args, context) {
  return createTask$2(args, {
    ...context,
    entities: {
      Task: dbClient.task
    }
  });
}

var createTask = createAction(createTask$1);

async function getMyTasks$1(args, context) {
  return getMyTasks$2(args, {
    ...context,
    entities: {
      Task: dbClient.task
    }
  });
}

var getMyTasks = createQuery(getMyTasks$1);

const router$3 = express.Router();
router$3.post("/create-task", auth, createTask);
router$3.post("/get-my-tasks", auth, getMyTasks);

const _waspGlobalMiddlewareConfigFn = (mc) => mc;
const defaultGlobalMiddlewareConfig = /* @__PURE__ */ new Map([
  ["helmet", helmet()],
  ["cors", cors({ origin: config.allowedCORSOrigins })],
  ["logger", logger("dev")],
  ["express.json", express.json()],
  ["express.urlencoded", express.urlencoded()],
  ["cookieParser", cookieParser()]
]);
const globalMiddlewareConfig = _waspGlobalMiddlewareConfigFn(defaultGlobalMiddlewareConfig);
function globalMiddlewareConfigForExpress(middlewareConfigFn) {
  if (!middlewareConfigFn) {
    return Array.from(globalMiddlewareConfig.values());
  }
  const globalMiddlewareConfigClone = new Map(globalMiddlewareConfig);
  const modifiedMiddlewareConfig = middlewareConfigFn(globalMiddlewareConfigClone);
  return Array.from(modifiedMiddlewareConfig.values());
}

var me = defineHandler(async (req, res) => {
  if (req.user) {
    res.json(serialize(req.user));
  } else {
    res.json(serialize(null));
  }
});

var logout = defineHandler(async (req, res) => {
  if (req.sessionId) {
    await invalidateSession(req.sessionId);
    res.json({ success: true });
  } else {
    throw createInvalidCredentialsError();
  }
});

var login = defineHandler(async (req, res) => {
  const providerId = req.params.providerId;
  if (!isExternalAuthProviderId(providerId)) {
    throw new HttpError(404, `Unknown auth provider '${String(providerId ?? "")}'.`);
  }
  const session = await exchangeRequestForSession(providerId, req);
  if (session === null) {
    throw createInvalidCredentialsError();
  }
  res.json({ sessionId: session.id });
});
function isExternalAuthProviderId(providerId) {
  return typeof providerId === "string" && providerId in externalAuthProviders;
}

const providers = [];
const router$2 = Router();
for (const provider of providers) {
  const { createRouter } = provider;
  const providerRouter = createRouter(provider);
  router$2.use(`/${provider.id}`, providerRouter);
  console.log(`\u{1F680} "${provider.displayName}" auth initialized`);
}

const router$1 = express.Router();
router$1.get("/me", auth, me);
router$1.post("/logout", auth, logout);
router$1.post("/login/:providerId", login);
router$1.use("/", router$2);

const makeWrongPortPage = ({
  appName,
  frontendUrl
}) => (
  /* HTML */
  `
  <!doctype html>
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>${appName} API Server</title>

      <style>
        :root {
          --page-background: #f0f0f0;
          --wrapper-background: white;
          --wasp-yellow: #f5cc05;
          --main-link-color: #1a73e8;
        }

        .wrapper {
          font-family: system-ui, sans-serif;
          width: 90%;
          max-width: 600px;
          margin: 2em auto;
        }

        h1,
        h2 {
          margin: 0;
        }

        .main-link {
          text-align: center;
          font-size: 1.5em;
          font-weight: bold;
          font-family: ui-monospace, monospace;
        }

        .icon {
          width: 1em;
          height: 1em;
        }

        .wasp-title {
          margin: 0.5em 0;
          display: flex;
          align-items: center;
          gap: 0.2em;
        }

        body {
          background-color: var(--page-background);
        }

        main {
          background-color: var(--wrapper-background);
          padding: 1.5em;
          border-radius: 10px;
        }

        a,
        a:visited {
          color: var(--main-link-color);
        }
      </style>
    </head>
    <body>
      <div class="wrapper">
        <header>
          <h2 class="wasp-title">
            <svg viewBox="0 0 161 161" class="icon" alt="Wasp Logo">
              <circle cx="80.5" cy="80.5" r="79" fill="var(--wasp-yellow)" />
              <path
                d="M88.67 114.33h2.91q6 0 7.87-1.89c1.22-1.25 1.83-3.9 1.83-7.93V93.89c0-4.46.65-7.7 1.93-9.73s3.51-3.43 6.67-4.2q-4.69-1.08-6.65-4.12c-1.3-2-2-5.28-2-9.77V55.44q0-6-1.83-7.93t-7.87-1.88h-2.86V39.5h2.65q10.65 0 14.24 3.15t3.59 12.62v10.29c0 4.28.77 7.24 2.29 8.87s4.3 2.44 8.32 2.44h2.74V83h-2.74q-6 0-8.32 2.49c-1.52 1.65-2.29 4.64-2.29 9v10.25q0 9.47-3.59 12.64t-14.24 3.12h-2.65Z"
              />
              <path d="M38.5 85.15h37.33v7.58H38.5Zm0-17.88h37.33v7.49H38.5Z" />
            </svg>
            Wasp
          </h2>
        </header>

        <main>
          <h1>${appName} API Server</h1>
          <p>
            The server is up and running. This is the backend part of your Wasp
            application.
          </p>
          <p>
            If you want to visit your frontend application, go to this URL in
            your browser:
          </p>
          <a href="${frontendUrl}" class="main-link">
            <p>${frontendUrl}</p>
          </a>
          <p>
            <small>
              This message is shown because you are running the server in
              development mode. In production, this route would not show
              anything.
            </small>
          </p>
        </main>
      </div>
    </body>
  </html>
`
);

const router = express.Router();
const middleware = globalMiddlewareConfigForExpress();
router.get(
  "/",
  middleware,
  function(_req, res) {
    const data = {
      appName: "authProviderWaspAuthLib",
      frontendUrl: config.frontendUrl
    };
    const wrongPortPage = makeWrongPortPage(data);
    res.status(200).type("html").send(wrongPortPage);
  }
);
router.use("/auth", middleware, router$1);
const authProviderMiddleware_0 = globalMiddlewareConfigForExpress((middlewareConfig) => {
  return middlewareConfig;
});
router.use("/wasp-auth", authProviderMiddleware_0, (req, res, next) => {
  const routeHandler = authProviderRouteHandlers["external:wasp-auth"];
  if (routeHandler === void 0) {
    return next(new Error("The manifest of auth provider 'external:wasp-auth' declares routes, but its server adapter returned no routeHandler."));
  }
  return Promise.resolve(routeHandler(req, res)).catch(next);
});
router.use("/operations", middleware, router$3);

const app = express();
app.use("/", router);
app.use((err, _req, res, next) => {
  if (res.headersSent) {
    return next(err);
  }
  if (err instanceof HttpError) {
    return res.status(err.statusCode).json({ message: err.message, data: err.data });
  }
  return next(err);
});

const startServer = async () => {
  const port = normalizePort(config.port);
  app.set("port", port);
  const server = http.createServer(app);
  server.listen(port);
  server.on("error", (error) => {
    if (error.syscall !== "listen") throw error;
    const bind = typeof port === "string" ? "Pipe " + port : "Port " + port;
    switch (error.code) {
      case "EACCES":
        console.error(bind + " requires elevated privileges");
        process.exit(1);
      case "EADDRINUSE":
        console.error(bind + " is already in use");
        process.exit(1);
      default:
        throw error;
    }
  });
  server.on("listening", () => {
    const addr = server.address();
    const bind = typeof addr === "string" ? "pipe " + addr : "port " + addr.port;
    console.log("Server listening on " + bind);
  });
};
startServer().catch((e) => console.error(e));
function normalizePort(val) {
  const port = parseInt(val, 10);
  if (isNaN(port)) return val;
  if (port >= 0) return port;
  return false;
}
//# sourceMappingURL=server.js.map
