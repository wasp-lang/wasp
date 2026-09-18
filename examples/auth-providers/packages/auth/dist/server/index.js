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
 * window (the `identity-namespaces` grant, the credentials facet, plus
 * `email-send` when the email method is on), the serializable options the
 * spec helper captured, and the user-code extensions the manifest
 * referenced, delivered through virtual modules. The route handler mounts
 * at `/auth/<scheme>`.
 */
export const createServerAuthHandler = (runtime, options, extensions) => {
    const ctx = {
        runtime,
        options,
        extensions: groupExtensions(extensions ?? {}),
    };
    const routes = [
        ...(options.methods.usernameAndPassword !== undefined
            ? usernameRoutes(ctx)
            : []),
        ...(options.methods.email !== undefined ? emailRoutes(ctx) : []),
        ...oauthRoutes(ctx),
        // Account linking between the enabled methods: the per-method link
        // routes live with their methods, the shared ones here.
        ...linkingRoutes(ctx, OAUTH_PROVIDER_NAMES.some((name) => options.methods[name] !== undefined)),
    ];
    if (options.methods.email !== undefined) {
        boundEmailHelpers = makeEmailHelpers(runtime);
    }
    // The routes above verify logins; the credential a request carries
    // afterwards belongs to the credentials scheme (this scheme's private
    // issuer by default). Authentication forwards there, the way ASP.NET's
    // remote schemes forward to their sign-in scheme, so `authRequired`
    // naming this scheme recognizes the credentials it handed out.
    const handler = {
        authenticate: (request) => runtime.credentials.authenticate(request),
        signOut: (request) => runtime.credentials.signOut(request),
    };
    return { handler, routeHandler: makeDispatcher(routes) };
};
/**
 * The manifest delivers user functions as a flat record keyed the way the
 * spec helper named them (`emailUserSignupFields`, `googleConfigFn`, ...);
 * the flows read them grouped by kind.
 */
function groupExtensions(flat) {
    const grouped = {
        userSignupFields: {},
        configFns: {},
        getVerificationEmailContent: flat.getVerificationEmailContent,
        getPasswordResetEmailContent: flat.getPasswordResetEmailContent,
        onAfterEmailVerified: flat.onAfterEmailVerified,
        onBeforeOAuthRedirect: flat.onBeforeOAuthRedirect,
    };
    for (const method of [
        "username",
        "email",
        ...OAUTH_PROVIDER_NAMES,
    ]) {
        const fields = flat[`${method}UserSignupFields`];
        if (fields !== undefined) {
            grouped.userSignupFields[method] = fields;
        }
    }
    for (const name of OAUTH_PROVIDER_NAMES) {
        const configFn = flat[`${name}ConfigFn`];
        if (configFn !== undefined) {
            grouped.configFns[name] = configFn;
        }
    }
    return grouped;
}
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
