import { parseCookies } from "@wasp.sh/lib-auth/node";
import { generateCodeVerifier, generateState } from "arctic";

import { findAuthWithUser } from "../email/flows.js";
import {
  HttpError,
  getBody,
  getUrl,
  isHttpErrorLike,
  json,
  redirect,
  type Route,
} from "../http.js";
import { DEFAULT_ROUTES_BASE_PATH, namespaceFor } from "../namespaces.js";
import type { Ctx, OAuthData, OAuthProviderName, Req, Res } from "../types.js";
import {
  TimeSpan,
  makeJwt,
  rethrowPossibleAuthError,
  validateAndGetUserFields,
} from "../utils.js";
import {
  makeOAuthProvider,
  type OAuthProviderDefinition,
} from "./providers.js";

export const OAUTH_PROVIDER_NAMES: OAuthProviderName[] = [
  "google",
  "github",
  "slack",
  "discord",
  "keycloak",
  "microsoft",
];

const LOGIN_PATH = "login";
const CALLBACK_PATH = "callback";
const EXCHANGE_CODE_PATH = "/exchange-code";

/**
 * The OAuth methods: `/auth/<provider>/login`, `/auth/<provider>/callback`
 * and the shared `/auth/exchange-code` redemption -- the in-tree handler,
 * state/cookie and one-time-code machinery, on the contract's facets.
 */
export function oauthRoutes(ctx: Ctx): Route[] {
  const { runtime, options } = ctx;
  const enabled = OAUTH_PROVIDER_NAMES.filter(
    (name) => options.methods[name] !== undefined,
  );
  if (enabled.length === 0) {
    return [];
  }
  const jwt = makeJwt(runtime);

  const routes: Route[] = enabled.flatMap((name) => {
    const provider = makeOAuthProvider(
      runtime,
      name,
      `${runtime.serverUrl}${options.routesBasePath ?? DEFAULT_ROUTES_BASE_PATH}/${name}/${CALLBACK_PATH}`,
    );
    const config = mergeDefaultAndUserConfig(
      { scopes: options.methods[name]!.requiredScopes },
      ctx.extensions.configFns?.[name],
    );
    return [
      {
        method: "GET" as const,
        path: `/${name}/${LOGIN_PATH}`,
        handler: (req: Req, res: Res) =>
          loginHandler(ctx, provider, config, req, res),
      },
      {
        method: "GET" as const,
        path: `/${name}/${CALLBACK_PATH}`,
        handler: (req: Req, res: Res) =>
          callbackHandler(ctx, provider, config, jwt, req, res),
      },
    ];
  });

  routes.push({
    method: "POST",
    path: EXCHANGE_CODE_PATH,
    handler: async (req, res) => {
      const { code } = getBody(req);
      if (typeof code !== "string") {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The code is missing.",
        );
      }
      const { sessionId } = await jwt
        .validateJWT<{ sessionId: string }>(code)
        .catch(() => {
          throw new HttpError(
            400,
            "Unable to login with the OAuth provider. The code is invalid.",
          );
        });
      // Spending the code BEFORE answering settles concurrent redemptions:
      // exactly one caller gets `true`, whichever instance it hit.
      if (!(await tryMarkCodeUsed(runtime, code))) {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The code has already been used.",
        );
      }
      json(res, 200, { sessionId });
    },
  });

  return routes;
}

function mergeDefaultAndUserConfig(
  defaultConfig: { scopes: string[] },
  userConfigFn?: () => Record<string, unknown>,
): { scopes: string[] } & Record<string, unknown> {
  return userConfigFn ? { ...defaultConfig, ...userConfigFn() } : defaultConfig;
}

async function loginHandler(
  ctx: Ctx,
  provider: OAuthProviderDefinition,
  config: { scopes: string[] },
  req: Req,
  res: Res,
): Promise<void> {
  const state = {
    state: generateState(),
    ...(provider.oAuthType === "OAuth2WithPKCE"
      ? { codeVerifier: generateCodeVerifier() }
      : {}),
  };
  storeOAuthState(ctx, provider, res, state);
  const redirectUrl = await provider.getAuthorizationUrl(state, config);
  let url = redirectUrl;
  if (ctx.extensions.onBeforeOAuthRedirect) {
    const result = (await ctx.extensions.onBeforeOAuthRedirect({
      prisma: ctx.runtime.db,
      req,
      url: redirectUrl,
      oauth: { uniqueRequestId: state.state },
    })) as { url: URL } | undefined;
    url = result?.url ?? redirectUrl;
  }
  redirect(res, url.toString());
}

async function callbackHandler(
  ctx: Ctx,
  provider: OAuthProviderDefinition,
  config: { scopes: string[] },
  jwt: ReturnType<typeof makeJwt>,
  req: Req,
  res: Res,
): Promise<void> {
  const { runtime, options, extensions } = ctx;
  try {
    const oAuthState = validateAndGetOAuthState(provider, req);
    const tokens = await provider.getProviderTokens(oAuthState);
    const { providerProfile, providerUserId } = await provider.getProviderInfo(
      tokens,
      config,
    );
    const oauth: OAuthData = {
      uniqueRequestId: oAuthState.state,
      providerName: provider.id,
      tokens,
    };

    const identities = runtime.identityNamespaces(namespaceFor(provider.id));
    const existing = await identities.find(providerUserId);
    let isNewUser = false;
    if (!existing) {
      try {
        // The facet's `create` fires the app's signup hooks (with the OAuth
        // tokens as their `oauth` payload) around the atomic write.
        await identities.create(
          providerUserId,
          {},
          (() =>
            validateAndGetUserFields(
              { profile: providerProfile },
              extensions.userSignupFields?.[provider.id],
            )) as never,
          { req, hookContext: oauth },
        );
        isNewUser = true;
      } catch (e) {
        rethrowPossibleAuthError(e);
      }
    }

    // The session is minted HERE, where the tokens exist, so the app's login
    // hooks receive them (skipped for a fresh signup, whose signup hooks just
    // fired -- the in-tree semantics). The one-time code then names the
    // session, and redeeming it is a plain hand-over.
    const { sessionId } = await runtime.sessions.issue(
      { namespace: namespaceFor(provider.id), subjectId: providerUserId },
      { req, hookContext: oauth, skipHooks: isNewUser },
    );
    const oneTimeCode = await jwt.createJWT(
      { sessionId },
      { expiresIn: new TimeSpan(1, "m") },
    );
    redirect(
      res,
      `${runtime.clientUrl}${options.clientOAuthCallbackPath}#${oneTimeCode}`,
    );
  } catch (error) {
    console.error(error);
    const message = isHttpErrorLike(error)
      ? typeof (error.data as { message?: unknown } | undefined)?.message ===
        "string"
        ? `${error.message}: ${(error.data as { message: string }).message}`
        : error.message
      : "An unknown error occurred while trying to log in with the OAuth provider.";
    redirect(
      res,
      `${runtime.clientUrl}${options.clientOAuthCallbackPath}?error=${message}`,
    );
  }
}

// --- state cookies (the in-tree oauth/cookies.ts + state.ts) ---------------

function cookieName(provider: OAuthProviderDefinition, field: string): string {
  return `${provider.id}_${field}`;
}

function storeOAuthState(
  ctx: Ctx,
  provider: OAuthProviderDefinition,
  res: Res,
  state: Record<string, string>,
): void {
  const attributes = [
    "HttpOnly",
    "SameSite=Lax",
    "Path=/",
    "Max-Age=3600",
    ...(ctx.runtime.isDevelopment ? [] : ["Secure"]),
  ].join("; ");
  res.setHeader(
    "Set-Cookie",
    Object.entries(state).map(
      ([field, value]) =>
        `${cookieName(provider, field)}=${value}; ${attributes}`,
    ),
  );
}

function validateAndGetOAuthState(
  provider: OAuthProviderDefinition,
  req: Req,
): { code: string; state: string; codeVerifier?: string } {
  const url = getUrl(req);
  const code = url.searchParams.get("code");
  const state = url.searchParams.get("state");
  const cookies = parseCookies(req.headers.cookie ?? "");
  const storedState = cookies.get(cookieName(provider, "state"));
  const codeVerifier =
    provider.oAuthType === "OAuth2WithPKCE"
      ? cookies.get(cookieName(provider, "codeVerifier"))
      : undefined;

  if (typeof code !== "string") throw new Error("Invalid code");
  if (!state || !storedState || storedState !== state)
    throw new Error("Invalid state");
  if (provider.oAuthType === "OAuth2WithPKCE" && !codeVerifier)
    throw new Error("Missing code verifier");
  return { code, state, codeVerifier };
}

// --- one-time-code replay protection --------------------------------------

/**
 * Spends a one-time code by inserting it into the `UsedOneTimeCode` table
 * Wasp's schema injection provides: true exactly once per code, whichever
 * server instance redeems it. Stale rows are cleaned up lazily.
 */
async function tryMarkCodeUsed(
  runtime: Ctx["runtime"],
  code: string,
): Promise<boolean> {
  const db = runtime.db as {
    usedOneTimeCode: {
      create(args: { data: { code: string } }): Promise<unknown>;
      deleteMany(args: { where: { usedAt: { lt: Date } } }): Promise<unknown>;
    };
  };
  await db.usedOneTimeCode.deleteMany({
    where: { usedAt: { lt: new Date(Date.now() - 1000 * 60 * 60) } },
  });
  try {
    await db.usedOneTimeCode.create({ data: { code } });
    return true;
  } catch (e) {
    if (
      typeof e === "object" &&
      e !== null &&
      (e as { code?: unknown }).code === "P2002"
    ) {
      return false;
    }
    throw e;
  }
}

export { findAuthWithUser };
