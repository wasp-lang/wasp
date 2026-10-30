/**
 * Sample data for testing the spec pipeline.
 * Modeled on __tests__/legacy/testFixtures.ts; scoped to what the spec
 * surface currently supports (`page`, `query`).
 */
import * as AppSpec from "../../src/appSpec.js";
import { Branded } from "../../src/branded.js";
import * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";
import type { AnyFunction } from "../../src/typeUtils.js";
export declare const MOCK_PROJECT_DIR = "/project";
export declare const MOCK_MAIN_WASP_TS_PATH = "/project/main.wasp.ts";
export declare function getApp(scope: ConfigScope): WaspSpec.App;
export declare function getMinimalAppWithSpec(spec: WaspSpec.Spec): WaspSpec.App;
export declare function getPage<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Page>;
export declare function getRoute<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Route>;
export declare function getQuery<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Query>;
export declare function getAction<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Action>;
export declare function getApi<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Api>;
export declare function getApiNamespace<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.ApiNamespace>;
export declare function getJob<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Job>;
export declare function getCrud<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Crud>;
export declare function getCrudOperations<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.CrudOperations>;
export declare function getCrudOperationOptions<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.CrudOperationOptions>;
export declare function getSchedule<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Schedule>;
export declare function getEntities(scope: ConfigScope): string[];
export declare function getServerConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Server>;
export declare function getClientConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Client>;
export declare function getDbConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Db>;
export declare function getEmailSenderConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.EmailSender>;
export declare function getWebSocketConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.WebSocket>;
export declare function getAuthConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.Auth>;
export declare function getAuthMethods<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.AuthMethods>;
export declare function getUsernameAndPasswordConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.UsernameAndPasswordConfig>;
export declare function getSocialAuthConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.SocialAuthConfig>;
export declare function getEmailAuthConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.EmailAuthConfig>;
export declare function getEmailVerificationConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.EmailFlowConfig>;
export declare function getPasswordResetConfig<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.EmailFlowConfig>;
export declare function getEmailFromField<Scope extends ConfigScope>(scope: Scope): ConfigFor<Scope, WaspSpec.EmailFromField>;
export declare const EMAIL_VERIFY_ROUTE_PATH = "/email-verify";
export declare const EMAIL_VERIFY_ROUTE_NAME = "EmailVerifyRoute";
export declare const PASSWORD_RESET_ROUTE_PATH = "/password-reset";
export declare const PASSWORD_RESET_ROUTE_NAME = "PasswordResetRoute";
export declare function getEmailVerifyRoute(): WaspSpec.Route;
export declare function getPasswordResetRoute(): WaspSpec.Route;
export declare function getRefObject<Scope extends ConfigScope, Kind extends AppSpec.ExtImportKind>(scope: Scope, importKind: Kind): ConfigFor<Scope, RefObjectFor<Kind>>;
export type Config<T> = MinimalConfig<T> | FullConfig<T>;
/**
 * Recursively strips optional properties from T.
 * - Branded types are passed through (don't recurse into the brand).
 * - Arrays recurse into element type.
 * - Objects keep only required keys (collapse to EmptyObject when none remain).
 * - Primitives pass through.
 * - Functions pass through.
 */
export type MinimalConfig<T> = T extends Branded<unknown, unknown> ? T : T extends AnyFunction ? T : T extends Array<infer Item> ? Array<MinimalConfig<Item>> : T extends object ? keyof T extends never ? EmptyObject : MinimalConfigObject<T> : T;
type MinimalConfigObject<T> = {
    [K in keyof T as EmptyObject extends Pick<T, K> ? never : K]: MinimalConfig<T[K]>;
} extends infer Result ? Result extends EmptyObject ? EmptyObject : Result : never;
type EmptyObject = Record<string, never>;
/**
 * Recursively makes every property of T required.
 * - Branded types are passed through (don't recurse into the brand).
 * - Arrays recurse into element type.
 * - Objects strip every `?` and recurse.
 * - Primitives pass through.
 * - Functions pass through.
 *
 * Exception: keys that can only ever be `never` (the exclusion markers
 * generated by `RequireOneOrNone` & friends, e.g. `usernameAndPassword?: never`
 * on the email branch of `AuthMethods`) are left optional. Forcing them to be
 * required would make the type unsatisfiable.
 */
export type FullConfig<T> = T extends Branded<unknown, unknown> ? T : T extends AnyFunction ? T : T extends Array<infer Item> ? Array<FullConfig<Item>> : T extends object ? FullConfigObject<T> : T;
type FullConfigObject<T> = {
    [K in keyof T as IsExclusionMarker<T[K]> extends true ? never : K]-?: FullConfig<T[K]>;
} & {
    [K in keyof T as IsExclusionMarker<T[K]> extends true ? K : never]?: T[K];
};
type IsExclusionMarker<V> = [Exclude<V, undefined>] extends [never] ? true : false;
type RefObjectFor<Kind extends AppSpec.ExtImportKind> = Kind extends "named" ? WaspSpec.RefObject & WaspSpec.NamedRefObjectDescriptor : WaspSpec.RefObject & WaspSpec.DefaultRefObjectDescriptor;
type ConfigFor<Scope extends ConfigScope, Data> = Scope extends "full" ? FullConfig<Data> : MinimalConfig<Data>;
type ConfigScope = "minimal" | "full";
export {};
//# sourceMappingURL=testFixtures.d.ts.map