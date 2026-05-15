import { describe, expectTypeOf, test } from "vitest";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import type { AnyFunction } from "../../src/typeUtils.js";

describe("ExtImport input types", () => {
  test("should accept functions at ExtImport use sites", async () => {
    const component = () => null;
    const operation = async (_args: { id: string }) => null;
    const hook = async () => null;
    const setup = () => null;
    const middleware = () => null;

    const pageConfig = { kind: "page", component } satisfies TsAppSpec.Page;
    const queryConfig = {
      kind: "query",
      fn: operation,
    } satisfies TsAppSpec.Query;
    const actionConfig = {
      kind: "action",
      fn: operation,
    } satisfies TsAppSpec.Action;
    const apiConfig = {
      kind: "api",
      method: "GET",
      path: "/api",
      fn: operation,
      middlewareConfigFn: middleware,
    } satisfies TsAppSpec.Api;
    const apiNamespaceConfig = {
      kind: "apiNamespace",
      path: "/api",
      middlewareConfigFn: middleware,
    } satisfies TsAppSpec.ApiNamespace;
    const jobConfig = {
      kind: "job",
      fn: operation,
      executor: "PgBoss",
    } satisfies TsAppSpec.Job;
    const webSocketConfig = { fn: operation } satisfies TsAppSpec.WebSocket;
    const authConfig = {
      userEntity: "User",
      methods: {},
      onAuthFailedRedirectTo: "/login",
      onBeforeSignup: hook,
    } satisfies TsAppSpec.Auth;
    const externalAuthConfig = {
      configFn: hook,
      userSignupFields: hook,
    } satisfies TsAppSpec.ExternalAuthConfig;
    const emailFlowConfig = {
      getEmailContentFn: hook,
      clientRoute: "EmailRoute",
    } satisfies TsAppSpec.EmailFlowConfig;
    const serverConfig = {
      setupFn: setup,
      middlewareConfigFn: middleware,
    } satisfies TsAppSpec.Server;
    const clientConfig = {
      rootComponent: component,
      setupFn: setup,
    } satisfies TsAppSpec.Client;
    const dbConfig = {
      seeds: [hook],
      prismaSetupFn: setup,
    } satisfies TsAppSpec.Db;

    expectTypeOf(pageConfig.component).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(queryConfig.fn).toExtend<TsAppSpec.ExtImport | AnyFunction>();
    expectTypeOf(actionConfig.fn).toExtend<TsAppSpec.ExtImport | AnyFunction>();
    expectTypeOf(apiConfig.fn).toExtend<TsAppSpec.ExtImport | AnyFunction>();
    expectTypeOf(apiConfig.middlewareConfigFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(apiNamespaceConfig.middlewareConfigFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(jobConfig.fn).toExtend<TsAppSpec.ExtImport | AnyFunction>();
    expectTypeOf(webSocketConfig.fn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(authConfig.onBeforeSignup).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(externalAuthConfig.configFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(externalAuthConfig.userSignupFields).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(emailFlowConfig.getEmailContentFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(serverConfig.setupFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(serverConfig.middlewareConfigFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(clientConfig.rootComponent).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(clientConfig.setupFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(dbConfig.seeds).toExtend<
      (TsAppSpec.ExtImport | AnyFunction)[]
    >();
    expectTypeOf(dbConfig.prismaSetupFn).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
  });

  test("should reject objects that are not ExtImport objects at ExtImport use sites", async () => {
    const component = { render: () => null };

    const pageConfig: TsAppSpec.Page = {
      kind: "page",
      // @ts-expect-error ExtImport use sites accept descriptors or functions.
      component,
    };

    expectTypeOf(pageConfig).toExtend<TsAppSpec.Page>();
  });

  test("should reject malformed descriptor-like objects", async () => {
    const pageConfig: TsAppSpec.Page = {
      kind: "page",
      // @ts-expect-error descriptor-like objects must still satisfy ExtImport.
      component: { from: 42, importDefault: "X" },
    };

    expectTypeOf(pageConfig).toExtend<TsAppSpec.Page>();
  });
});

describe("Env validation schema input types", () => {
  test("should accept Zod object schema-shaped values", async () => {
    const schema = {
      shape: {},
      safeParse: (_data: unknown) => ({ success: true, data: {} }),
      and: (_schema: unknown) => schema,
    };

    const serverConfig = {
      envValidationSchema: schema,
    } satisfies TsAppSpec.Server;
    const clientConfig = {
      envValidationSchema: schema,
    } satisfies TsAppSpec.Client;

    expectTypeOf(serverConfig.envValidationSchema).toExtend<
      TsAppSpec.ExtImport | TsAppSpec.ZodObjectSchema
    >();
    expectTypeOf(clientConfig.envValidationSchema).toExtend<
      TsAppSpec.ExtImport | TsAppSpec.ZodObjectSchema
    >();
  });

  test("should accept ExtImport env validation schemas", async () => {
    const schemaImport = {
      importDefault: "schema",
      from: "@src/env",
    } satisfies TsAppSpec.ExtImport;

    const serverConfig = {
      envValidationSchema: schemaImport,
    } satisfies TsAppSpec.Server;
    const clientConfig = {
      envValidationSchema: schemaImport,
    } satisfies TsAppSpec.Client;

    expectTypeOf(serverConfig.envValidationSchema).toExtend<
      TsAppSpec.ExtImport | TsAppSpec.ZodObjectSchema
    >();
    expectTypeOf(clientConfig.envValidationSchema).toExtend<
      TsAppSpec.ExtImport | TsAppSpec.ZodObjectSchema
    >();
  });

  test("should reject non-schema objects at env validation schema use sites", async () => {
    const serverConfig: TsAppSpec.Server = {
      // @ts-expect-error Env validation schema objects must be Zod object schema-shaped.
      envValidationSchema: {},
    };
    const clientConfig: TsAppSpec.Client = {
      // @ts-expect-error Env validation schema objects must be Zod object schema-shaped.
      envValidationSchema: [],
    };

    expectTypeOf(serverConfig).toExtend<TsAppSpec.Server>();
    expectTypeOf(clientConfig).toExtend<TsAppSpec.Client>();
  });
});
