import { assertType, describe, test } from "vitest";
import { _waspMakeRef } from "../../src/internal.js";
import { ref } from "../../src/spec/publicApi/index.js";
describe("RefObject input types", () => {
    test("should accept ref helper output at reference use sites", () => {
        const component = ref({
            importDefault: "MainPage",
            from: "./MainPage",
        });
        assertType({ kind: "page", component });
    });
    test("should accept _waspMakeRef helper output at reference use sites", () => {
        const sourceAwareRefImport = _waspMakeRef(import.meta.url);
        const component = sourceAwareRefImport({
            importDefault: "MainPage",
            from: "./MainPage",
        });
        assertType({ kind: "page", component });
    });
    test("should accept functions at reference use sites", () => {
        const component = () => null;
        const operation = async (_args) => null;
        const object = { field: () => "" };
        const hook = async () => null;
        const setup = () => null;
        const middleware = () => null;
        assertType({ kind: "page", component });
        assertType({
            kind: "query",
            fn: operation,
        });
        assertType({
            kind: "action",
            fn: operation,
        });
        assertType({
            kind: "api",
            method: "GET",
            path: "/api",
            fn: operation,
            middlewareConfigFn: middleware,
        });
        assertType({
            kind: "apiNamespace",
            path: "/api",
            middlewareConfigFn: middleware,
        });
        assertType({
            kind: "job",
            fn: operation,
            executor: "PgBoss",
        });
        assertType({ fn: operation });
        assertType({
            userEntity: "User",
            methods: {},
            onAuthFailedRedirectTo: "/login",
            onBeforeSignup: hook,
        });
        assertType({
            configFn: hook,
            userSignupFields: object,
        });
        assertType({
            getEmailContentFn: hook,
            clientRoute: "EmailRoute",
        });
        assertType({
            setupFn: setup,
            middlewareConfigFn: middleware,
        });
        assertType({
            rootComponent: component,
            setupFn: setup,
        });
        assertType({
            seeds: [hook],
            prismaSetupFn: setup,
        });
    });
    test("should reject objects that are not RefObject objects at reference use sites", () => {
        const component = { render: () => null };
        // @ts-expect-error Reference use sites accept RefObject values or functions.
        assertType({ kind: "page", component });
    });
    test("should reject raw descriptor-like objects", () => {
        const component = { from: "./MainPage", importDefault: "MainPage" };
        // @ts-expect-error Descriptor-like objects must be wrapped in ref.
        assertType({ kind: "page", component });
    });
    test("should reject incomplete RefObject objects", () => {
        const component = { kind: "refObject", from: "./MainPage" };
        // @ts-expect-error RefObject objects must include either import or importDefault.
        assertType({ kind: "page", component });
    });
    test("should reject handwritten RefObject-shaped objects", () => {
        const component = {
            kind: "refObject",
            importDefault: "MainPage",
            from: "./MainPage",
        };
        // @ts-expect-error RefObject objects must come from ref or reference imports.
        assertType({ kind: "page", component });
    });
});
describe("Env validation schema input types", () => {
    test("should accept Zod schema-shaped values", () => {
        const schema = {
            _zod: {
                def: {
                    type: "string",
                },
            },
        };
        assertType({ envValidationSchema: schema });
        assertType({ envValidationSchema: schema });
    });
    test("should accept RefObject env validation schemas", () => {
        const schemaImport = ref({
            importDefault: "schema",
            from: "./env",
        });
        assertType({ envValidationSchema: schemaImport });
        assertType({ envValidationSchema: schemaImport });
    });
    test("should reject raw descriptor env validation schemas", () => {
        const schemaImport = {
            importDefault: "schema",
            from: "./env",
        };
        // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
        assertType({ envValidationSchema: schemaImport });
        // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
        assertType({ envValidationSchema: schemaImport });
    });
    test("should reject non-schema objects at env validation schema use sites", () => {
        // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
        assertType({ envValidationSchema: {} });
        // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
        assertType({ envValidationSchema: [] });
    });
    test("should reject malformed Zod schema-shaped objects", () => {
        // @ts-expect-error Zod schema-shaped values must include a definition object.
        assertType({ envValidationSchema: { _zod: true } });
        // @ts-expect-error Zod schema-shaped values must include a definition object.
        assertType({ envValidationSchema: { _zod: {} } });
    });
});
