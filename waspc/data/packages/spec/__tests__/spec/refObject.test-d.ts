import { assertType, describe, test } from "vitest";
import { _waspMakeRef, ref } from "../../src/spec/publicApi/index.js";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

describe("RefObject input types", () => {
  test("should accept ref helper output at reference use sites", () => {
    const component = ref({
      importDefault: "MainPage",
      from: "./MainPage",
    });

    assertType<TsAppSpec.Page>({ kind: "page", component });
  });

  test("should accept _waspMakeRef helper output at reference use sites", () => {
    const sourceAwareRefImport = _waspMakeRef(import.meta.url);
    const component = sourceAwareRefImport({
      importDefault: "MainPage",
      from: "./MainPage",
    });

    assertType<TsAppSpec.Page>({ kind: "page", component });
  });

  test("should accept functions at reference use sites", () => {
    const component = () => null;
    const operation = async (_args: { id: string }) => null;
    const object = { field: () => "" };
    const hook = async () => null;
    const setup = () => null;
    const middleware = () => null;

    assertType<TsAppSpec.Page>({ kind: "page", component });
    assertType<TsAppSpec.Query>({
      kind: "query",
      fn: operation,
    });
    assertType<TsAppSpec.Action>({
      kind: "action",
      fn: operation,
    });
    assertType<TsAppSpec.Api>({
      kind: "api",
      method: "GET",
      path: "/api",
      fn: operation,
      middlewareConfigFn: middleware,
    });
    assertType<TsAppSpec.ApiNamespace>({
      kind: "apiNamespace",
      path: "/api",
      middlewareConfigFn: middleware,
    });
    assertType<TsAppSpec.Job>({
      kind: "job",
      fn: operation,
      executor: "PgBoss",
    });
    assertType<TsAppSpec.WebSocket>({ fn: operation });
    assertType<TsAppSpec.Auth>({
      userEntity: "User",
      methods: {},
      onAuthFailedRedirectTo: "/login",
      onBeforeSignup: hook,
    });
    assertType<TsAppSpec.SocialAuthConfig>({
      configFn: hook,
      userSignupFields: object,
    });
    assertType<TsAppSpec.EmailFlowConfig>({
      getEmailContentFn: hook,
      clientRoute: "EmailRoute",
    });
    assertType<TsAppSpec.Server>({
      setupFn: setup,
      middlewareConfigFn: middleware,
    });
    assertType<TsAppSpec.Client>({
      rootComponent: component,
      setupFn: setup,
    });
    assertType<TsAppSpec.Db>({
      seeds: [hook],
      prismaSetupFn: setup,
    });
  });

  test("should reject objects that are not RefObject objects at reference use sites", () => {
    const component = { render: () => null };

    // @ts-expect-error Reference use sites accept RefObject values or functions.
    assertType<TsAppSpec.Page>({ kind: "page", component });
  });

  test("should reject raw descriptor-like objects", () => {
    const component = { from: "./MainPage", importDefault: "MainPage" };

    // @ts-expect-error Descriptor-like objects must be wrapped in ref.
    assertType<TsAppSpec.Page>({ kind: "page", component });
  });

  test("should reject incomplete RefObject objects", () => {
    const component = { kind: "refObject", from: "./MainPage" } as const;

    // @ts-expect-error RefObject objects must include either import or importDefault.
    assertType<TsAppSpec.Page>({ kind: "page", component });
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
    } satisfies TsAppSpec.ZodSchema;

    assertType<TsAppSpec.Server>({ envValidationSchema: schema });
    assertType<TsAppSpec.Client>({ envValidationSchema: schema });
  });

  test("should accept RefObject env validation schemas", () => {
    const schemaImport = ref({
      importDefault: "schema",
      from: "./env",
    });

    assertType<TsAppSpec.Server>({ envValidationSchema: schemaImport });
    assertType<TsAppSpec.Client>({ envValidationSchema: schemaImport });
  });

  test("should reject raw descriptor env validation schemas", () => {
    const schemaImport = {
      importDefault: "schema",
      from: "./env",
    };

    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<TsAppSpec.Server>({ envValidationSchema: schemaImport });

    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<TsAppSpec.Client>({ envValidationSchema: schemaImport });
  });

  test("should reject non-schema objects at env validation schema use sites", () => {
    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<TsAppSpec.Server>({ envValidationSchema: {} });

    // @ts-expect-error Env validation schemas must use ref or Zod schema-shaped values.
    assertType<TsAppSpec.Client>({ envValidationSchema: [] });
  });

  test("should reject malformed Zod schema-shaped objects", () => {
    // @ts-expect-error Zod schema-shaped values must include a definition object.
    assertType<TsAppSpec.Server>({ envValidationSchema: { _zod: true } });

    // @ts-expect-error Zod schema-shaped values must include a definition object.
    assertType<TsAppSpec.Client>({ envValidationSchema: { _zod: {} } });
  });
});
