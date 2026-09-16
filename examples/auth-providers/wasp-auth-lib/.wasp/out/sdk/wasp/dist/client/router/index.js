import { interpolatePath } from './linkHelpers';
// PUBLIC API
export const routes = {
    MainRoute: {
        to: "/",
        build: (options) => interpolatePath("/", undefined, options?.search, options?.hash),
    },
    LoginRoute: {
        to: "/login",
        build: (options) => interpolatePath("/login", undefined, options?.search, options?.hash),
    },
    OAuthCallbackRoute: {
        to: "/oauth/callback",
        build: (options) => interpolatePath("/oauth/callback", undefined, options?.search, options?.hash),
    },
    EmailVerifiedRoute: {
        to: "/email-verified",
        build: (options) => interpolatePath("/email-verified", undefined, options?.search, options?.hash),
    },
    PasswordResetRoute: {
        to: "/password-reset",
        build: (options) => interpolatePath("/password-reset", undefined, options?.search, options?.hash),
    },
};
// PUBLIC API
export { Link } from './Link';
// PUBLIC API
export { NavLink } from './NavLink';
//# sourceMappingURL=index.js.map