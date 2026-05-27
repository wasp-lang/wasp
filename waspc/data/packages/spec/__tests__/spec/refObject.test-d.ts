import { assertType, describe, test } from "vitest";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

describe("RefObject input types", () => {
  test("should accept functions at RefObject use sites", () => {
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

  test("should reject objects that are not RefObjects at RefObject use sites", () => {
    const component = { render: () => null };

    // @ts-expect-error RefObject use sites accept descriptors or functions.
    assertType<TsAppSpec.Page>({ kind: "page", component });
  });

  test("should reject malformed descriptor-like objects", () => {
    const component = { from: 42, import: "default", alias: "X" };

    // @ts-expect-error Descriptor-like objects must still satisfy RefObject.
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
    const schemaImport = {
      import: "default",
      alias: "schema",
      from: "@src/env",
    } satisfies TsAppSpec.RefObject;

    assertType<TsAppSpec.Server>({ envValidationSchema: schemaImport });
    assertType<TsAppSpec.Client>({ envValidationSchema: schemaImport });
  });

  test("should reject non-schema objects at env validation schema use sites", () => {
    // @ts-expect-error Env validation schemas must be RefObject or Zod schema-shaped.
    assertType<TsAppSpec.Server>({ envValidationSchema: {} });

    // @ts-expect-error Env validation schemas must be RefObject or Zod schema-shaped.
    assertType<TsAppSpec.Client>({ envValidationSchema: [] });
  });

  test("should reject malformed Zod schema-shaped objects", () => {
    // @ts-expect-error Zod schema-shaped values must include a definition object.
    assertType<TsAppSpec.Server>({ envValidationSchema: { _zod: true } });

    // @ts-expect-error Zod schema-shaped values must include a definition object.
    assertType<TsAppSpec.Client>({ envValidationSchema: { _zod: {} } });
  });
});
