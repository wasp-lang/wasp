import type { RouteDefinitionsToRoutes, OptionalRouteOptions } from './types';
export declare const routes: {
    readonly MainRoute: {
        readonly to: "/";
        readonly build: (options?: OptionalRouteOptions) => string;
    };
    readonly LoginRoute: {
        readonly to: "/login";
        readonly build: (options?: OptionalRouteOptions) => string;
    };
    readonly OAuthCallbackRoute: {
        readonly to: "/oauth/callback";
        readonly build: (options?: OptionalRouteOptions) => string;
    };
    readonly EmailVerifiedRoute: {
        readonly to: "/email-verified";
        readonly build: (options?: OptionalRouteOptions) => string;
    };
    readonly PasswordResetRoute: {
        readonly to: "/password-reset";
        readonly build: (options?: OptionalRouteOptions) => string;
    };
};
export type Routes = RouteDefinitionsToRoutes<typeof routes>;
export { Link } from './Link';
export { NavLink } from './NavLink';
//# sourceMappingURL=index.d.ts.map