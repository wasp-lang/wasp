import { assertType, describe, test } from "vitest";
import { _waspMakeRef } from "../../src/internal.js";
import { ref } from "../../src/spec/publicApi/index.js";
import type * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";

describe("RefObject input types", () => {
  test("should accept ref helper output at reference use sites", () => {
    const component = ref({
      importDefault: "MainPage",
      from: "./MainPage",
    });

    assertType<WaspSpec.Page>({ kind: "page", component });
  });

  test("should accept _waspMakeRef helper output at reference use sites", () => {
    const originAwareRefImport = _waspMakeRef({
      kind: "project",
      specFilePath: "main.wasp.ts",
    });
    const component = originAwareRefImport({
      importDefault: "MainPage",
      from: "./MainPage",
    });

    assertType<WaspSpec.Page>({ kind: "page", component });
  });

  test("should accept functions at reference use sites", () => {
    const component = () => null;
    const operation = async (_args: { id: string }) => null;
    const object = { field: () => "" };
    const hook = async () => null;
    const setup = () => null;
    const middleware = () => null;

    assertType<WaspSpec.Page>({ kind: "page", component });
    assertType<WaspSpec.Query>({
      kind: "query",
      fn: operation,
    });
    assertType<WaspSpec.Action>({
      kind: "action",
      fn: operation,
    });
    assertType<WaspSpec.Api>({
      kind: "api",
      method: "GET",
      path: "/api",
      fn: operation,
      middlewareConfigFn: middleware,
    });
    assertType<WaspSpec.ApiNamespace>({
      kind: "apiNamespace",
      path: "/api",
      middlewareConfigFn: middleware,
    });
    assertType<WaspSpec.Job>({
      kind: "job",
      fn: operation,
      executor: "PgBoss",
    });
    assertType<WaspSpec.WebSocket>({ fn: operation });
    assertType<WaspSpec.Auth>({
      userEntity: "User",
      methods: {},
      onAuthFailedRedirectTo: "/login",
      onBeforeSignup: hook,
    });
    assertType<WaspSpec.SocialAuthConfig>({
      configFn: hook,
      userSignupFields: object,
    });
    assertType<WaspSpec.EmailFlowConfig>({
      getEmailContentFn: hook,
      clientRoute: "EmailRoute",
    });
    assertType<WaspSpec.Server>({
      setupFn: setup,
      middlewareConfigFn: middleware,
    });
    assertType<WaspSpec.Client>({
      rootComponent: component,
      setupFn: setup,
    });
    assertType<WaspSpec.Db>({
      seeds: [hook],
      prismaSetupFn: setup,
    });
  });

  test("should reject objects that are not RefObject objects at reference use sites", () => {
    const component = { render: () => null };

    // @ts-expect-error Reference use sites accept RefObject values or functions.
    assertType<WaspSpec.Page>({ kind: "page", component });
  });

  test("should reject raw descriptor-like objects", () => {
    const component = { from: "./MainPage", importDefault: "MainPage" };

    // @ts-expect-error Descriptor-like objects must be wrapped in ref.
    assertType<WaspSpec.Page>({ kind: "page", component });
  });

  test("should reject incomplete RefObject objects", () => {
    const component = { kind: "refObject", from: "./MainPage" } as const;

    // @ts-expect-error RefObject objects must include either import or importDefault.
    assertType<WaspSpec.Page>({ kind: "page", component });
  });

  test("should reject handwritten RefObject-shaped objects", () => {
    const component = {
      kind: "refObject",
      importDefault: "MainPage",
      from: "./MainPage",
    } as const;

    // @ts-expect-error RefObject objects must come from ref or reference imports.
    assertType<WaspSpec.Page>({ kind: "page", component });
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
    } satisfies WaspSpec.ZodSchema;

    assertType<WaspSpec.Server>({ envValidationSchema: schema });
    assertType<WaspSpec.Client>({ envValidationSchema: schema });
  });

  test("should accept RefObject env validation schemas", () => {
    const schemaImport = ref({
      importDefault: "schema",
      from: "./env",
    });

    assertType<WaspSpec.Server>({ envValidationSchema: schemaImport });
    assertType<WaspSpec.Client>({ envValidationSchema: schemaImport });
  });

  test("should reject raw descriptor env validation schemas", () => {
    const schemaImport = {
      importDefault: "schema",
      from: "./env",
    };

    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<WaspSpec.Server>({ envValidationSchema: schemaImport });

    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<WaspSpec.Client>({ envValidationSchema: schemaImport });
  });

  test("should reject non-schema objects at env validation schema use sites", () => {
    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<WaspSpec.Server>({ envValidationSchema: {} });

    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<WaspSpec.Client>({ envValidationSchema: [] });
  });

  test("should reject malformed Zod schema-shaped objects", () => {
    // @ts-expect-error Zod schema-shaped values must include a definition object.
    assertType<WaspSpec.Server>({ envValidationSchema: { _zod: true } });

    // @ts-expect-error Zod schema-shaped values must include a definition object.
    assertType<WaspSpec.Client>({ envValidationSchema: { _zod: {} } });
  });
});
