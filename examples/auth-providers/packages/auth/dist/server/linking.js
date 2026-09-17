import { getAuthContractErrorCode } from "@wasp.sh/auth-contract";
import { HttpError, getBody, json } from "./http.js";
import { namespaceFor } from "./namespaces.js";
import { TimeSpan, makeJwt } from "./utils.js";
const METHOD_NAMES = [
    "username",
    "email",
    "google",
    "github",
    "slack",
    "discord",
    "keycloak",
    "microsoft",
];
/**
 * The account of the signed-in user making this request. Goes through the
 * credentials facet, so it works whatever issuer this scheme signs into
 * (its private one, or a sibling `waspBearer()` / `waspCookie()`), bearer or
 * cookie. For a Wasp-issued credential the principal's subject IS the Auth id.
 */
export async function requireCurrentAuthId({ runtime }, req) {
    const headers = new Headers();
    for (const [name, value] of Object.entries(req.headers)) {
        if (value !== undefined) {
            headers.set(name, Array.isArray(value) ? value.join(", ") : value);
        }
    }
    const result = await runtime.credentials.authenticate(new Request(`${runtime.serverUrl}${req.url ?? "/"}`, { headers }));
    if (result.status !== "authenticated") {
        throw new HttpError(401, "Sign in before changing your connected accounts.");
    }
    return result.principal.subjectId;
}
/** Maps the facet's link/unlink rejections onto HTTP answers the client reads. */
export function rethrowLinkError(e) {
    switch (getAuthContractErrorCode(e)) {
        case "wasp-auth/identity-linked-elsewhere":
            throw new HttpError(409, "That login already belongs to another account.", {
                reason: "linked-elsewhere",
            });
        case "wasp-auth/last-identity":
            throw new HttpError(409, "You cannot disconnect your only login method.", {
                reason: "last-identity",
            });
        case "wasp-auth/identity-not-found":
            throw new HttpError(404, "Your account has no such login method.");
        case "wasp-auth/policy-veto":
            throw new HttpError(e.statusCode ?? 403, e.message);
        default:
            throw e;
    }
}
/**
 * Routes every method shares: `/unlink`, and `/link-intent` for the OAuth
 * methods. An OAuth link starts with a browser NAVIGATION, which cannot carry
 * a bearer credential; the client first trades its credential for a
 * short-lived signed ticket here, and the navigation carries that instead.
 */
export function linkingRoutes(ctx, hasOAuth) {
    const { runtime } = ctx;
    const routes = [
        {
            method: "POST",
            path: "/unlink",
            handler: async (req, res) => {
                const authId = await requireCurrentAuthId(ctx, req);
                const { method, subjectId } = getBody(req);
                if (typeof subjectId !== "string" ||
                    !METHOD_NAMES.includes(method)) {
                    throw new HttpError(400, "Expected a login method and its subject.");
                }
                try {
                    // The namespace guard rejects methods this scheme never declared.
                    await runtime
                        .identityNamespaces(namespaceFor(runtime, method))
                        .unlink(subjectId, { authId });
                }
                catch (e) {
                    rethrowLinkError(e);
                }
                json(res, 200, { success: true });
            },
        },
    ];
    if (hasOAuth) {
        routes.push({
            method: "POST",
            path: "/link-intent",
            handler: async (req, res) => {
                const ticket = {
                    linkToAuthId: await requireCurrentAuthId(ctx, req),
                };
                json(res, 200, {
                    ticket: await makeJwt(runtime).createJWT(ticket, {
                        expiresIn: new TimeSpan(10, "m"),
                    }),
                });
            },
        });
    }
    return routes;
}
