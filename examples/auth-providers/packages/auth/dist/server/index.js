import { emailRoutes } from "./email/flows.js";
import { isEmailResendAllowed, makeEmailHelpers, } from "./email/utils.js";
import { makeDispatcher } from "./http.js";
import { linkingRoutes } from "./linking.js";
import { oauthRoutes } from "./oauth/index.js";
import { usernameRoutes } from "./username.js";
const OAUTH_PROVIDER_NAMES = [
    "google",
    "github",
    "slack",
    "discord",
    "keycloak",
    "microsoft",
];
/**
 * Wasp's own authentication as an auth handler package.
 *
 * Wasp instantiates this exactly like any handler package: with the runtime
 * window (the credentials issuer, plus the `email-send` grant when the email
 * method is on) and the `server.spec` the spec constructor captured, with the
 * app's functions live in place. The route handler mounts
 * at `/auth/<scheme>`.
 */
export const createServerAuthHandler = (runtime, spec) => {
    const ctx = { runtime, spec };
    const routes = [
        ...(spec.methods.usernameAndPassword !== undefined
            ? usernameRoutes(ctx)
            : []),
        ...(spec.methods.email !== undefined ? emailRoutes(ctx) : []),
        ...oauthRoutes(ctx),
        // Account linking between the enabled methods: the per-method link
        // routes live with their methods, the shared ones here.
        ...linkingRoutes(ctx, OAUTH_PROVIDER_NAMES.some((name) => spec.methods[name] !== undefined)),
    ];
    if (spec.methods.email !== undefined) {
        boundEmailHelpers = makeEmailHelpers(runtime);
    }
    // The routes above verify logins; the credential a request carries
    // afterwards belongs to the credentials scheme (this scheme's private
    // issuer by default). Authentication forwards there, the way ASP.NET's
    // remote schemes forward to their sign-in scheme, so `authRequired`
    // naming this scheme recognizes the credentials it handed out.
    const handler = {
        authenticate: (request) => runtime.credentialsIssuer.authenticate(request),
        signOut: (request) => runtime.credentialsIssuer.signOut(request),
    };
    return { handler, routeHandler: makeDispatcher(routes) };
};
// The email helpers (link builders, senders), bound to the runtime at handler
// creation. User code imports them from `@wasp.sh/auth/server`.
let boundEmailHelpers = null;
function getEmailHelpers() {
    if (boundEmailHelpers === null) {
        throw new Error("Wasp's email auth method is not enabled.");
    }
    return boundEmailHelpers;
}
export const createEmailVerificationLink = (...args) => getEmailHelpers().createEmailVerificationLink(...args);
export const createPasswordResetLink = (...args) => getEmailHelpers().createPasswordResetLink(...args);
export const sendEmailVerificationEmail = (...args) => getEmailHelpers().sendEmailVerificationEmail(...args);
export const sendPasswordResetEmail = (...args) => getEmailHelpers().sendPasswordResetEmail(...args);
export { isEmailResendAllowed };
export { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";
export { getEmail, getUsername } from "../user.js";
export { HttpError } from "./http.js";
export { ensurePasswordIsPresent, ensureTokenIsPresent, ensureValidEmail, ensureValidPassword, ensureValidUsername, } from "./validation.js";
