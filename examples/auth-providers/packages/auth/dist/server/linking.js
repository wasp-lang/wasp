import { getAuthContractErrorCode } from "@wasp.sh/auth-contract";
import { HttpError, getBody, json } from "./http.js";
import { anyIdentities, identitiesOf } from "./providerNames.js";
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
 * The account of the signed-in user making this request, as Wasp sees it:
 * whatever issuer this scheme signs into, bearer or cookie.
 */
export async function requireCurrentAuthId({ runtime }, req) {
    const account = await runtime.authenticate(req.request);
    if (account === null) {
        throw new HttpError(401, "Sign in before changing your connected accounts.");
    }
    return account.authId;
}
/** Maps the facet's link/unlink rejections onto HTTP answers the client reads. */
export function rethrowLinkError(e) {
    switch (getAuthContractErrorCode(e)) {
        case "wasp-auth/identity-linked-elsewhere":
            throw new HttpError(409, "That login already belongs to another account.", {
                reason: "linked-elsewhere",
            });
        case "wasp-auth/merging-disabled":
            throw new HttpError(409, "This app does not support merging accounts.");
        case "wasp-auth/credential-not-fresh":
            throw new HttpError(403, "Log in again before merging accounts: your current login is not recent enough.", { reason: "credential-not-fresh" });
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
const MERGE_TICKET_LIFETIME = new TimeSpan(10, "m");
/**
 * The link failed because the login belongs to another account. When the app
 * turned merging on AND the caller has just proven control of that login,
 * answer "merge required" with a signed ticket instead; the client confirms
 * with the user and posts it to `/merge`.
 *
 * `proveControl` is what makes this safe: without it any signed-in user
 * could absorb any account by naming its login. An unproven attempt falls
 * through to the ordinary "linked elsewhere" answer, so this is no oracle
 * for guessing another account's password.
 */
export async function offerMergeOrRethrow(ctx, e, attempt) {
    const { runtime } = ctx;
    if (getAuthContractErrorCode(e) === "wasp-auth/identity-linked-elsewhere" &&
        runtime.isAccountMergingEnabled) {
        const fromAuthId = await attempt.findFromAuthId();
        if (fromAuthId !== null && (await attempt.proveControl())) {
            throw mergeRequiredError(await createMergeTicket(ctx, {
                fromAuthId,
                intoAuthId: attempt.intoAuthId,
            }));
        }
    }
    rethrowLinkError(e);
}
export function createMergeTicket({ runtime }, ticket) {
    return makeJwt(runtime).createJWT(ticket, {
        expiresIn: MERGE_TICKET_LIFETIME,
    });
}
function mergeRequiredError(mergeTicket) {
    return new HttpError(409, "That login belongs to another account of yours. Merge the two?", { reason: "merge-required", mergeTicket });
}
/**
 * Routes every method shares: `/unlink`, and `/link-intent` for the OAuth
 * methods. An OAuth link starts with a browser NAVIGATION, which cannot carry
 * a bearer credential; the client first trades its credential for a one-time
 * code here, and the navigation carries that instead. Wasp issues the code,
 * and none under a cookie credential, which a navigation carries by itself,
 * so nothing here knows the transport.
 */
export function linkingRoutes(ctx, hasOAuth) {
    const { runtime } = ctx;
    const routes = [
        {
            method: "POST",
            path: "/unlink",
            handler: async (req, res) => {
                const authId = await requireCurrentAuthId(ctx, req);
                const { method, providerUserId } = getBody(req);
                if (typeof providerUserId !== "string" ||
                    !METHOD_NAMES.includes(method)) {
                    throw new HttpError(400, "Expected a login method and its subject.");
                }
                // A method this scheme never declared has no store.
                const methodIdentities = identitiesOf(runtime, method);
                if (methodIdentities === undefined) {
                    throw new HttpError(400, "This login method is not enabled.");
                }
                try {
                    await methodIdentities.unlink(providerUserId, { authId });
                }
                catch (e) {
                    rethrowLinkError(e);
                }
                json(res, 200, { success: true });
            },
        },
    ];
    routes.push({
        // The second step of a merge: the signed-in user confirmed. Only the
        // account the ticket was issued TO may redeem it.
        method: "POST",
        path: "/merge",
        handler: async (req, res) => {
            const authId = await requireCurrentAuthId(ctx, req);
            const { mergeTicket } = getBody(req);
            if (typeof mergeTicket !== "string") {
                throw new HttpError(400, "Expected a merge ticket.");
            }
            const ticket = await makeJwt(runtime)
                .validateJWT(mergeTicket)
                .catch(() => {
                throw new HttpError(400, "The merge request expired. Try again.");
            });
            if (ticket.intoAuthId !== authId) {
                throw new HttpError(403, "This merge was started by another account.");
            }
            try {
                await anyIdentities(runtime).merge({
                    fromAuthId: ticket.fromAuthId,
                    intoAuthId: ticket.intoAuthId,
                });
            }
            catch (e) {
                rethrowLinkError(e);
            }
            json(res, 200, { success: true });
        },
    });
    if (hasOAuth) {
        routes.push({
            method: "POST",
            path: "/link-intent",
            handler: async (req, res) => {
                const oneTimeCode = await runtime.credentialsIssuer
                    .createOneTimeCode(req.request)
                    .catch((e) => {
                    if (getAuthContractErrorCode(e) === "wasp-auth/unauthenticated") {
                        throw new HttpError(401, "Sign in before changing your connected accounts.");
                    }
                    throw e;
                });
                json(res, 200, { oneTimeCode });
            },
        });
    }
    return routes;
}
