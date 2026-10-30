/**
 * Sample data for testing the spec pipeline.
 * Modeled on __tests__/legacy/testFixtures.ts; scoped to what the spec
 * surface currently supports (`page`, `query`).
 */
import { _waspMakeRef } from "../../src/internal.js";
import { action, api, apiNamespace, app, job, page, query, route, } from "../../src/spec/publicApi/index.js";
export const MOCK_PROJECT_DIR = "/project";
export const MOCK_MAIN_WASP_TS_PATH = `${MOCK_PROJECT_DIR}/main.wasp.ts`;
export function getApp(scope) {
    switch (scope) {
        case "minimal":
            return app({
                name: "MinimalApp",
                wasp: { version: "^0.16.3" },
                title: "Mock App",
                spec: [],
            });
        case "full":
            return app({
                name: "FullApp",
                wasp: { version: "^0.16.3" },
                title: "Mock App",
                head: ['<link rel="icon" href="/favicon.ico" />'],
                auth: getAuthConfig("full"),
                server: getServerConfig("full"),
                client: getClientConfig("full"),
                db: getDbConfig("full"),
                emailSender: getEmailSenderConfig("full"),
                webSocket: getWebSocketConfig("full"),
                spec: [
                    getPage("full"),
                    getRoute("full"),
                    getQuery("full"),
                    getJob("full"),
                    getCrud("full"),
                    getEmailVerifyRoute(),
                    getPasswordResetRoute(),
                ],
            });
        default:
            assertUnreachable(scope);
    }
}
export function getMinimalAppWithSpec(spec) {
    return {
        ...getApp("minimal"),
        spec,
    };
}
export function getPage(scope) {
    switch (scope) {
        case "minimal":
            return page(getRefObject("minimal", "named"));
        case "full":
            return page(getRefObject("full", "named"), {
                authRequired: true,
            });
        default:
            assertUnreachable(scope);
    }
}
export function getRoute(scope) {
    switch (scope) {
        case "minimal":
            return route("minimalRoute", "/foo/bar", getPage("minimal"));
        case "full":
            return route("fullRoute", "/foo/bar", getPage("full"), {
                lazy: true,
                prerender: true,
            });
        default:
            assertUnreachable(scope);
    }
}
export function getQuery(scope) {
    switch (scope) {
        case "minimal":
            return query(getRefObject("minimal", "named"));
        case "full":
            return query(getRefObject("full", "named"), {
                entities: ["Task"],
                auth: true,
            });
        default:
            assertUnreachable(scope);
    }
}
export function getAction(scope) {
    switch (scope) {
        case "minimal":
            return action(getRefObject("minimal", "named"));
        case "full":
            return action(getRefObject("full", "named"), {
                entities: ["Task"],
                auth: true,
            });
        default:
            assertUnreachable(scope);
    }
}
export function getApi(scope) {
    switch (scope) {
        case "minimal":
            return api("GET", "/foo/bar", getRefObject("minimal", "named"));
        case "full":
            return api("POST", "/foo/bar", getRefObject("full", "named"), {
                middlewareConfigFn: getRefObject("full", "named"),
                entities: ["Task"],
                auth: true,
            });
        default:
            assertUnreachable(scope);
    }
}
export function getApiNamespace(scope) {
    switch (scope) {
        case "minimal":
            return apiNamespace("/foo", {
                middlewareConfigFn: getRefObject("minimal", "named"),
            });
        case "full":
            return apiNamespace("/foo", {
                middlewareConfigFn: getRefObject("full", "named"),
            });
        default:
            assertUnreachable(scope);
    }
}
export function getJob(scope) {
    switch (scope) {
        case "minimal":
            return job(getRefObject("minimal", "named"), {
                executor: "PgBoss",
            });
        case "full":
            return job(getRefObject("full", "named"), {
                executor: "PgBoss",
                schedule: getSchedule("full"),
                entities: ["Task"],
                performExecutorOptions: { pgBoss: { jobOptions: { attempts: 3 } } },
            });
        default:
            assertUnreachable(scope);
    }
}
export function getCrud(scope) {
    // NOTE: Unlike the other Spec Elements, this fixture builds the object literally
    // instead of calling the `crud()` constructor. `crud()` returns the public
    // `Crud` type, whose `operations: CrudOperations` (all fields optional)
    // satisfies neither `MinimalConfig<Crud>` (operations collapse to an empty
    // object) nor `FullConfig<Crud>` (operations become fully required). Building
    // the literal lets us keep the public type narrow, which is simpler for
    // consumers.
    switch (scope) {
        case "minimal":
            return {
                kind: "crud",
                name: "minimalCrud",
                entity: "Task",
                operations: getCrudOperations("minimal"),
            };
        case "full":
            return {
                kind: "crud",
                name: "fullCrud",
                entity: "Task",
                operations: getCrudOperations("full"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getCrudOperations(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                get: getCrudOperationOptions("full"),
                getAll: getCrudOperationOptions("full"),
                create: getCrudOperationOptions("full"),
                update: getCrudOperationOptions("full"),
                delete: getCrudOperationOptions("full"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getCrudOperationOptions(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                isPublic: true,
                overrideFn: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getSchedule(scope) {
    switch (scope) {
        case "minimal":
            return { cron: "0 0 * * *" };
        case "full":
            return {
                cron: "0 0 * * *",
                args: { foo: "bar" },
                executorOptions: { pgBoss: { jobOptions: { attempts: 3 } } },
            };
        default:
            assertUnreachable(scope);
    }
}
export function getEntities(scope) {
    switch (scope) {
        case "minimal":
            return [];
        case "full":
            return ["Task", "User", "SocialUser"];
        default:
            assertUnreachable(scope);
    }
}
export function getServerConfig(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                setupFn: getRefObject("full", "named"),
                middlewareConfigFn: getRefObject("full", "named"),
                envValidationSchema: getRefObject("full", "named"),
                traceDeps: ["some-native-pkg*", "!aws-crt"],
            };
        default:
            assertUnreachable(scope);
    }
}
export function getClientConfig(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                rootComponent: getRefObject("full", "named"),
                setupFn: getRefObject("full", "named"),
                baseDir: "/src",
                envValidationSchema: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getDbConfig(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                seeds: [getRefObject("full", "named"), getRefObject("full", "default")],
                prismaSetupFn: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getEmailSenderConfig(scope) {
    switch (scope) {
        case "minimal":
            return {
                provider: "SMTP",
            };
        case "full":
            return {
                provider: "SMTP",
                defaultFrom: getEmailFromField("full"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getWebSocketConfig(scope) {
    switch (scope) {
        case "minimal":
            return {
                fn: getRefObject("minimal", "named"),
            };
        case "full":
            return {
                fn: getRefObject("full", "named"),
                autoConnect: true,
            };
        default:
            assertUnreachable(scope);
    }
}
export function getAuthConfig(scope) {
    switch (scope) {
        case "minimal":
            return {
                userEntity: "User",
                methods: getAuthMethods("minimal"),
                onAuthFailedRedirectTo: "/login",
            };
        case "full":
            return {
                userEntity: "User",
                methods: getAuthMethods("full"),
                onAuthFailedRedirectTo: "/login",
                onAuthSucceededRedirectTo: "/profile",
                onBeforeSignup: getRefObject("full", "named"),
                onAfterSignup: getRefObject("full", "named"),
                onAfterEmailVerified: getRefObject("full", "named"),
                onBeforeOAuthRedirect: getRefObject("full", "named"),
                onBeforeLogin: getRefObject("full", "named"),
                onAfterLogin: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getAuthMethods(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                slack: getSocialAuthConfig("full"),
                discord: getSocialAuthConfig("full"),
                google: getSocialAuthConfig("full"),
                gitHub: getSocialAuthConfig("full"),
                keycloak: getSocialAuthConfig("full"),
                microsoft: getSocialAuthConfig("full"),
                email: getEmailAuthConfig("full"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getUsernameAndPasswordConfig(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                userSignupFields: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getSocialAuthConfig(scope) {
    switch (scope) {
        case "minimal":
            return {};
        case "full":
            return {
                configFn: getRefObject("full", "named"),
                userSignupFields: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getEmailAuthConfig(scope) {
    switch (scope) {
        case "minimal":
            return {
                fromField: getEmailFromField("minimal"),
                emailVerification: getEmailVerificationConfig("minimal"),
                passwordReset: getPasswordResetConfig("minimal"),
            };
        case "full":
            return {
                fromField: getEmailFromField("full"),
                emailVerification: getEmailVerificationConfig("full"),
                passwordReset: getPasswordResetConfig("full"),
                userSignupFields: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getEmailVerificationConfig(scope) {
    switch (scope) {
        case "minimal":
            return {
                clientRoute: EMAIL_VERIFY_ROUTE_NAME,
            };
        case "full":
            return {
                clientRoute: EMAIL_VERIFY_ROUTE_NAME,
                getEmailContentFn: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getPasswordResetConfig(scope) {
    switch (scope) {
        case "minimal":
            return {
                clientRoute: PASSWORD_RESET_ROUTE_NAME,
            };
        case "full":
            return {
                clientRoute: PASSWORD_RESET_ROUTE_NAME,
                getEmailContentFn: getRefObject("full", "named"),
            };
        default:
            assertUnreachable(scope);
    }
}
export function getEmailFromField(scope) {
    switch (scope) {
        case "minimal":
            return {
                email: "noreply@example.com",
            };
        case "full":
            return {
                name: "Wasp",
                email: "noreply@example.com",
            };
        default:
            assertUnreachable(scope);
    }
}
export const EMAIL_VERIFY_ROUTE_PATH = "/email-verify";
export const EMAIL_VERIFY_ROUTE_NAME = "EmailVerifyRoute";
export const PASSWORD_RESET_ROUTE_PATH = "/password-reset";
export const PASSWORD_RESET_ROUTE_NAME = "PasswordResetRoute";
export function getEmailVerifyRoute() {
    return route(EMAIL_VERIFY_ROUTE_NAME, EMAIL_VERIFY_ROUTE_PATH, page(getRefObjectForMockProject({
        import: "EmailVerifyPage",
        from: "./src/auth/pages",
    })));
}
export function getPasswordResetRoute() {
    return route(PASSWORD_RESET_ROUTE_NAME, PASSWORD_RESET_ROUTE_PATH, page(getRefObjectForMockProject({
        import: "PasswordResetPage",
        from: "./src/auth/pages",
    })));
}
export function getRefObject(scope, importKind) {
    switch (importKind) {
        case "named":
            return scope === "full"
                ? getRefObjectForMockProject({
                    import: "namedExport",
                    alias: "namedAlias",
                    from: "./src/external",
                })
                : getRefObjectForMockProject({
                    import: "namedExport",
                    from: "./src/external",
                });
        case "default":
            return getRefObjectForMockProject({
                importDefault: "defaultExport",
                from: "./src/external",
            });
        default:
            assertUnreachable(importKind);
    }
}
const getRefObjectForMockProject = _waspMakeRef(MOCK_MAIN_WASP_TS_PATH);
function assertUnreachable(value) {
    throw new Error(`Unhandled case: ${value}`);
}
