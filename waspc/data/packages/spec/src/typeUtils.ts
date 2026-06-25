export type AnyFunction = (...args: never[]) => unknown;

export type ServerSetupFn = (context: any) => Promise<void> | void;
export type ClientSetupFn = () => Promise<void> | void;
export type MiddlewareConfigFn = (middlewareConfig: any) => any;
export type DbSeedFn = (prisma: any) => Promise<void> | void;
export type PrismaSetupFn = () => any;

export type GetEmailContentFn = (args: any) => { subject: string, text: string, html: string } | Promise<{ subject: string, text: string, html: string }>;

export type OAuthConfigFn = () => any;

export type OnBeforeSignupFn = (providerId: string, providerProfile: any) => any;
export type OnAfterSignupFn = (providerId: string, user: any, providerProfile: any) => any;
export type OnBeforeOAuthRedirectFn = (url: string, uniqueRequestId: string) => any;
export type OnBeforeLoginFn = (providerId: string, user: any) => any;
export type OnAfterLoginFn = (providerId: string, user: any) => any;
export type OnAfterEmailVerifiedFn = (email: string, user: any) => any;

export type ActionFn = (args: any, context: any) => any;
export type QueryFn = (args: any, context: any) => any;
export type ApiFn = (req: any, res: any, context: any) => any;
export type JobFn = (args: any, context: any) => any;
export type WebSocketFn = (io: any, context: any) => any;
export type PageComponent = (...args: any[]) => any;
export type CrudOverrideFn = (args: any, context: any) => any;

export type AnyObject = Record<PropertyKey, unknown>;
