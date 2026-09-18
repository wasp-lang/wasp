import { getAuthContractErrorCode } from "@wasp.sh/auth-contract";
import { parseCookies } from "@wasp.sh/lib-auth/node";
import { generateCodeVerifier, generateState } from "arctic";

import { findAuthWithUser } from "../email/flows.js";
import {
  HttpError,
  getBody,
  getUrl,
  isHttpErrorLike,
  redirect,
  sendAuthResponse,
  type Route,
} from "../http.js";
import {
  createMergeTicket,
  requireCurrentAuthId,
  rethrowLinkError,
  type LinkTicket,
} from "../linking.js";
import { namespaceFor } from "../namespaces.js";
import type {
  Ctx,
  OAuthData,
  OAuthProviderName,
  Req,
  Res,
  SignInResponse,
} from "../types.js";
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
  const { runtime, config } = ctx;
  const enabled = OAUTH_PROVIDER_NAMES.filter(
    (name) => config.methods[name] !== undefined,
  );
  if (enabled.length === 0) {
    return [];
  }
  const jwt = makeJwt(runtime);

  const routes: Route[] = enabled.flatMap((name) => {
    const provider = makeOAuthProvider(
      runtime,
      name,
      `${runtime.serverUrl}${runtime.mountPath}/${name}/${CALLBACK_PATH}`,
    );
    const oauthConfig = mergeDefaultAndUserConfig(
      { scopes: config.methods[name]!.requiredScopes },
      config.methods[name]!.configFn,
    );
    return [
      {
        method: "GET" as const,
        path: `/${name}/${LOGIN_PATH}`,
        handler: (req: Req, res: Res) =>
          loginHandler(ctx, provider, oauthConfig, jwt, req, res),
      },
      {
        method: "GET" as const,
        path: `/${name}/${CALLBACK_PATH}`,
        handler: (req: Req, res: Res) =>
          callbackHandler(ctx, provider, oauthConfig, jwt, req, res),
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
      const { response } = await jwt
        .validateJWT<{ response: SignInResponse }>(code)
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
      sendAuthResponse(res, response);
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
  oauthConfig: { scopes: string[] },
  jwt: ReturnType<typeof makeJwt>,
  req: Req,
  res: Res,
): Promise<void> {
  const state = {
    state: generateState(),
    ...(provider.oAuthType === "OAuth2WithPKCE"
      ? { codeVerifier: generateCodeVerifier() }
      : {}),
    ...(await getLinkTicketCookie(ctx, jwt, req)),
  };
  storeOAuthState(ctx, provider, res, state);
  const redirectUrl = await provider.getAuthorizationUrl(state, oauthConfig);
  let url = redirectUrl;
  if (ctx.config.onBeforeOAuthRedirect) {
    const result = (await ctx.config.onBeforeOAuthRedirect({
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
  oauthConfig: { scopes: string[] },
  jwt: ReturnType<typeof makeJwt>,
  req: Req,
  res: Res,
): Promise<void> {
  const { runtime, config } = ctx;
  try {
    const oAuthState = validateAndGetOAuthState(provider, req);
    const tokens = await provider.getProviderTokens(oAuthState);
    const { providerProfile, providerUserId } = await provider.getProviderInfo(
      tokens,
      oauthConfig,
    );
    const oauth: OAuthData = {
      uniqueRequestId: oAuthState.state,
      providerName: provider.id,
      tokens,
    };

    const identities = runtime.identityNamespaces(
      namespaceFor(runtime, provider.id),
    );

    // Account linking: the flow was started by a signed-in user, so the
    // provider's identity is attached to THAT account. No credential is
    // issued; the user keeps the one they have.
    if (oAuthState.linkTicket !== undefined) {
      const { linkToAuthId } = await jwt
        .validateJWT<LinkTicket>(oAuthState.linkTicket)
        .catch(() => {
          throw new HttpError(400, "The link request expired. Try again.");
        });
      try {
        await identities.link(
          providerUserId,
          {},
          { authId: linkToAuthId, req, hookContext: oauth },
        );
      } catch (e) {
        // The provider account belongs to another Wasp account. Completing
        // the provider's flow just now is the proof that it is the caller's
        // own, so with merging on, hand the client a ticket to confirm.
        const existing = await identities.find(providerUserId);
        if (
          getAuthContractErrorCode(e) ===
            "wasp-auth/identity-linked-elsewhere" &&
          runtime.isAccountMergingEnabled &&
          existing !== null
        ) {
          const mergeTicket = await createMergeTicket(ctx, {
            fromAuthId: existing.authId,
            intoAuthId: linkToAuthId,
          });
          redirect(
            res,
            `${runtime.clientUrl}${config.clientOAuthCallbackPath}?mergeTicket=${encodeURIComponent(mergeTicket)}`,
          );
          return;
        }
        rethrowLinkError(e);
      }
      redirect(
        res,
        `${runtime.clientUrl}${config.clientOAuthCallbackPath}?linked=${provider.id}`,
      );
      return;
    }

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
              config.methods[provider.id]?.userSignupFields,
            )) as never,
          { req, hookContext: oauth },
        );
        isNewUser = true;
      } catch (e) {
        rethrowPossibleAuthError(e);
      }
    }

    // The sign-in happens HERE, where the tokens exist, so the app's login
    // hooks receive them (skipped for a fresh signup, whose signup hooks just
    // fired -- the in-tree semantics). The one-time code then carries the
    // credentials scheme's answer, and redeeming it replays that answer to
    // the client: a bearer token in the body, or a Set-Cookie header.
    const { response } = await runtime.credentials.signIn(
      {
        namespace: namespaceFor(runtime, provider.id),
        subjectId: providerUserId,
      },
      { req, hookContext: oauth, skipHooks: isNewUser },
    );
    const oneTimeCode = await jwt.createJWT(
      { response },
      { expiresIn: new TimeSpan(1, "m") },
    );
    redirect(
      res,
      `${runtime.clientUrl}${config.clientOAuthCallbackPath}#${oneTimeCode}`,
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
      `${runtime.clientUrl}${config.clientOAuthCallbackPath}?error=${message}`,
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

/**
 * The link intent of a login navigation, as the state cookie field that
 * carries it to the callback. The ticket is SIGNED: a state cookie is
 * client-controlled, and a plain account id in it could be edited to attach
 * the provider's identity to somebody else's account.
 */
async function getLinkTicketCookie(
  ctx: Ctx,
  jwt: ReturnType<typeof makeJwt>,
  req: Req,
): Promise<{ linkTicket: string }> {
  const params = getUrl(req).searchParams;
  if (params.get("intent") !== "link") {
    // Overwrites a ticket left behind by an abandoned link attempt, so an
    // ordinary login is never mistaken for one.
    return { linkTicket: "" };
  }
  // Bearer transport: the client traded its credential for a ticket first.
  const ticket = params.get("ticket");
  if (ticket !== null) {
    await jwt.validateJWT<LinkTicket>(ticket).catch(() => {
      throw new HttpError(400, "The link request expired. Try again.");
    });
    return { linkTicket: ticket };
  }
  // Cookie transport: the navigation itself carries the credential.
  const linkTicket: LinkTicket = {
    linkToAuthId: await requireCurrentAuthId(ctx, req),
  };
  return {
    linkTicket: await jwt.createJWT(linkTicket, {
      expiresIn: new TimeSpan(10, "m"),
    }),
  };
}

function validateAndGetOAuthState(
  provider: OAuthProviderDefinition,
  req: Req,
): { code: string; state: string; codeVerifier?: string; linkTicket?: string } {
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
  const linkTicket =
    cookies.get(cookieName(provider, "linkTicket")) || undefined;
  return { code, state, codeVerifier, linkTicket };
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
