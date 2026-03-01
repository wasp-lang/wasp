import { describe, expect, test } from "vitest";
import { getModuleSpec, GET_TS_APP_SPEC } from "../src/_private.js";
import { AppDeclBuilder, Module } from "../src/publicApi/Module.js";
import { App } from "../src/publicApi/App.js";
import type * as TsAppSpec from "../src/publicApi/tsAppSpec.js";

describe("Module entity declarations", () => {
  test("stores field declarations when fields are provided", () => {
    const mod = new Module("test-pkg");
    mod.entity("Todo", {
      fields: {
        id: "Int @id @default(autoincrement())",
        text: "String",
        isDone: "Boolean @default(false)",
      },
    });

    const spec = getModuleSpec(mod);
    expect(spec.entityAliases.has("Todo")).toBe(true);
    expect(spec.entityDeclarations.get("Todo")).toEqual({
      fields: {
        id: "Int @id @default(autoincrement())",
        text: "String",
        isDone: "Boolean @default(false)",
      },
    });
  });

  test("does not store declaration when no fields are provided", () => {
    const mod = new Module("test-pkg");
    mod.entity("Todo");

    const spec = getModuleSpec(mod);
    expect(spec.entityAliases.has("Todo")).toBe(true);
    expect(spec.entityDeclarations.has("Todo")).toBe(false);
  });

  test("stores multiple entity declarations", () => {
    const mod = new Module("test-pkg");
    mod.entity("User", {
      fields: { id: "Int @id @default(autoincrement())", email: "String" },
    });
    mod.entity("Post", {
      fields: { id: "Int @id @default(autoincrement())", title: "String" },
    });

    const spec = getModuleSpec(mod);
    expect(spec.entityDeclarations.size).toBe(2);
    expect(spec.entityDeclarations.get("User")?.fields.email).toBe("String");
    expect(spec.entityDeclarations.get("Post")?.fields.title).toBe("String");
  });

  test("throws on duplicate entity alias", () => {
    const mod = new Module("test-pkg");
    mod.entity("Todo", { fields: { id: "Int @id" } });
    expect(() => mod.entity("Todo", { fields: { id: "Int @id" } })).toThrow(
      "Entity alias 'Todo' is already declared on this module.",
    );
  });
});

describe("Module", () => {
  test("collects page declarations and returns PageName", () => {
    const mod = new Module("test-pkg");
    const pageName = mod.page("MyPage", {
      component: { import: "MyPage", from: "@src/pages/MyPage" },
    });

    expect(pageName).toBe("MyPage");
    const spec = getModuleSpec(mod);
    expect(spec.pages.size).toBe(1);
    expect(spec.pages.get("MyPage")).toEqual({
      component: { import: "MyPage", from: "@src/pages/MyPage" },
    });
  });

  test("collects route declarations", () => {
    const mod = new Module("test-pkg");
    const pageName = mod.page("MyPage", {
      component: { import: "MyPage", from: "@src/pages/MyPage" },
    });
    mod.route("MyRoute", { path: "/my", to: pageName });

    const spec = getModuleSpec(mod);
    expect(spec.routes.size).toBe(1);
    expect(spec.routes.get("MyRoute")).toEqual({
      path: "/my",
      to: "MyPage",
    });
  });

  test("collects query declarations", () => {
    const mod = new Module("test-pkg");
    mod.query("getItems", {
      fn: { import: "getItems", from: "@src/queries" },
      entities: ["Item"],
    });

    const spec = getModuleSpec(mod);
    expect(spec.queries.size).toBe(1);
    expect(spec.queries.get("getItems")).toMatchObject({
      fn: { import: "getItems", from: "@src/queries" },
    });
  });

  test("collects action declarations", () => {
    const mod = new Module("test-pkg");
    mod.action("createItem", {
      fn: { import: "createItem", from: "@src/actions" },
    });

    const spec = getModuleSpec(mod);
    expect(spec.actions.size).toBe(1);
    expect(spec.actions.get("createItem")).toMatchObject({
      fn: { import: "createItem", from: "@src/actions" },
    });
  });

  test("collects api, apiNamespace, crud, and job declarations", () => {
    const mod = new Module("test-pkg");

    mod.api("myApi", {
      fn: { import: "myApi", from: "@src/apis" },
      httpRoute: { method: "GET", route: "/api/test" },
    });
    mod.apiNamespace("myNs", {
      middlewareConfigFn: { import: "mw", from: "@src/middleware" },
      path: "/api",
    });
    mod.crud("myCrud", {
      entity: "Task",
      operations: { get: { isPublic: true } },
    });
    mod.job("myJob", {
      executor: "PgBoss",
      perform: { fn: { import: "perform", from: "@src/jobs" } },
    });

    const spec = getModuleSpec(mod);
    expect(spec.apis.size).toBe(1);
    expect(spec.apiNamespaces.size).toBe(1);
    expect(spec.cruds.size).toBe(1);
    expect(spec.jobs.size).toBe(1);
  });
});

describe("Module path resolution", () => {
  test("passes through @src/ paths unchanged", () => {
    const mod = new Module("test-pkg");
    mod.page("MyPage", {
      component: { import: "MyPage", from: "@src/pages/MyPage" },
    });

    const spec = getModuleSpec(mod);
    expect(spec.pages.get("MyPage")).toEqual({
      component: { import: "MyPage", from: "@src/pages/MyPage" },
    });
  });

  test("throws on relative paths", () => {
    const mod = new Module("test-pkg");
    expect(() =>
      mod.query("q", {
        fn: { import: "q", from: "./queries.js" },
      })
    ).toThrow(/Must start with "@src\/"/);
  });

  test("throws on paths that escape with ../", () => {
    const mod = new Module("test-pkg");
    expect(() =>
      mod.query("q", {
        fn: { import: "q", from: "../outside.js" },
      })
    ).toThrow(/Must start with "@src\/"/);
  });

  test("throws on bare specifier paths", () => {
    const mod = new Module("test-pkg");
    expect(() =>
      mod.query("q", {
        fn: { import: "q", from: "bare-specifier" },
      })
    ).toThrow(/Invalid import path/);
  });

  test("resolves nested ExtImport in CrudConfig overrideFn", () => {
    const mod = new Module("test-pkg");
    mod.crud("myCrud", {
      entity: "Task",
      operations: {
        get: {
          isPublic: true,
          overrideFn: { import: "getOverride", from: "@src/queries" },
        },
      },
    });

    const spec = getModuleSpec(mod);
    const crudOps = spec.cruds.get("myCrud")?.operations;
    expect(crudOps?.get?.overrideFn).toEqual({
      import: "getOverride",
      from: "@src/queries",
    });
  });

  test("resolves ExtImport in job perform.fn", () => {
    const mod = new Module("test-pkg");
    mod.job("myJob", {
      executor: "PgBoss",
      perform: { fn: { import: "run", from: "@src/jobs" } },
    });

    const spec = getModuleSpec(mod);
    expect(spec.jobs.get("myJob")?.perform.fn).toEqual({
      import: "run",
      from: "@src/jobs",
    });
  });

  test("resolves ExtImport in api middlewareConfigFn", () => {
    const mod = new Module("test-pkg");
    mod.api("myApi", {
      fn: { import: "handler", from: "@src/apis" },
      middlewareConfigFn: { import: "mw", from: "@src/middleware" },
      httpRoute: { method: "GET", route: "/test" },
    });

    const spec = getModuleSpec(mod);
    const api = spec.apis.get("myApi");
    expect(api?.fn).toEqual({
      import: "handler",
      from: "@src/apis",
    });
    expect(api?.middlewareConfigFn).toEqual({
      import: "mw",
      from: "@src/middleware",
    });
  });
});

describe("AppDeclBuilder", () => {
  test("can be created without packageName", () => {
    const builder = new AppDeclBuilder();
    const spec = getModuleSpec(builder);
    expect(spec.packageName).toBeUndefined();
  });

  test("can be created with packageName", () => {
    const builder = new AppDeclBuilder("my-pkg");
    const spec = getModuleSpec(builder);
    expect(spec.packageName).toBe("my-pkg");
  });
});

describe("App.use()", () => {
  function createTestApp(): App {
    return new App("TestApp", {
      title: "Test",
      wasp: { version: "^0.16.0" },
    });
  }

  test("merges module declarations into app", () => {
    const app = createTestApp();
    const mod = new Module("test-pkg");

    mod.page("DashPage", {
      component: { import: "DashPage", from: "@src/pages/Dash" },
    });
    mod.route("DashRoute", {
      path: "/dash",
      to: "DashPage" as TsAppSpec.PageName,
    });
    mod.query("getData", {
      fn: { import: "getData", from: "@src/queries" },
    });

    app.use(mod);

    const spec = app[GET_TS_APP_SPEC]();
    expect(spec.pages.get("DashPage")).toBeDefined();
    expect(spec.routes.get("DashRoute")).toBeDefined();
    expect(spec.queries.get("getData")).toBeDefined();
  });

  test("throws on duplicate between app and module with provenance", () => {
    const app = createTestApp();
    app.page("SharedPage", {
      component: { import: "SharedPage", from: "@src/pages/Shared" },
    });

    const mod = new Module("@pkg/a");
    mod.page("SharedPage", {
      component: { import: "OtherPage", from: "@src/pages/Other" },
    });

    expect(() => app.use(mod)).toThrow(
      "Duplicate pages declaration: 'SharedPage' from '@pkg/a' conflicts with 'SharedPage' from 'app'.",
    );
  });

  test("throws on duplicate between two modules with provenance", () => {
    const app = createTestApp();
    const mod1 = new Module("@pkg/a");
    mod1.query("getData", {
      fn: { import: "getData", from: "@src/queries1" },
    });

    const mod2 = new Module("@pkg/b");
    mod2.query("getData", {
      fn: { import: "getData", from: "@src/queries2" },
    });

    app.use(mod1);
    expect(() => app.use(mod2)).toThrow(
      "Duplicate queries declaration: 'getData' from '@pkg/b' conflicts with 'getData' from '@pkg/a'.",
    );
  });

  test("can be called multiple times with different modules", () => {
    const app = createTestApp();

    const mod1 = new Module("@pkg/a");
    mod1.page("Page1", {
      component: { import: "Page1", from: "@src/pages/Page1" },
    });
    mod1.route("Route1", {
      path: "/page1",
      to: "Page1" as TsAppSpec.PageName,
    });

    const mod2 = new Module("@pkg/b");
    mod2.page("Page2", {
      component: { import: "Page2", from: "@src/pages/Page2" },
    });
    mod2.route("Route2", {
      path: "/page2",
      to: "Page2" as TsAppSpec.PageName,
    });

    app.use(mod1);
    app.use(mod2);

    const spec = app[GET_TS_APP_SPEC]();
    expect(spec.pages.size).toBe(2);
    expect(spec.routes.size).toBe(2);
  });

  test("page() returns PageName usable in route() config", () => {
    const mod = new Module("test-pkg");
    const pageName = mod.page("TestPage", {
      component: { import: "TestPage", from: "@src/pages/Test" },
    });
    mod.route("TestRoute", { path: "/test", to: pageName });

    const app = createTestApp();
    app.use(mod);

    const spec = app[GET_TS_APP_SPEC]();
    expect(spec.routes.get("TestRoute")?.to).toBe("TestPage");
  });

  test("module declarations appear in final GET_TS_APP_SPEC output alongside app singletons", () => {
    const app = createTestApp();
    app.auth({
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      methods: {},
    });

    const mod = new Module("test-pkg");
    mod.query("getUser", {
      fn: { import: "getUser", from: "@src/queries" },
    });

    app.use(mod);

    const spec = app[GET_TS_APP_SPEC]();
    expect(spec.auth).toBeDefined();
    expect(spec.app.name).toBe("TestApp");
    expect(spec.queries.get("getUser")).toBeDefined();
  });

  test("skips rewriting @src/ paths when module package matches projectPackageName", () => {
    const app = createTestApp();
    App.projectPackageName = "my-module";

    const mod = new Module("my-module");
    mod.query("getItems", {
      fn: { import: "getItems", from: "@src/queries" },
    });
    mod.page("ModPage", {
      component: { import: "ModPage", from: "@src/pages/Mod" },
    });

    app.use(mod);

    const spec = app[GET_TS_APP_SPEC]();
    expect(spec.queries.get("getItems")?.fn.from).toBe("@src/queries");
    expect(spec.pages.get("ModPage")?.component.from).toBe("@src/pages/Mod");

    App.projectPackageName = undefined;
  });

  test("rewrites @src/ paths when module package differs from projectPackageName", () => {
    const app = createTestApp();
    App.projectPackageName = "my-app";

    const mod = new Module("external-module");
    mod.query("getItems", {
      fn: { import: "getItems", from: "@src/queries" },
    });

    app.use(mod);

    const spec = app[GET_TS_APP_SPEC]();
    expect(spec.queries.get("getItems")?.fn.from).toBe(
      "@pkg/external-module/queries",
    );

    App.projectPackageName = undefined;
  });

  test("app direct declarations and module declarations coexist", () => {
    const app = createTestApp();
    app.page("AppPage", {
      component: { import: "AppPage", from: "@src/pages/App" },
    });

    const mod = new Module("test-pkg");
    mod.page("ModPage", {
      component: { import: "ModPage", from: "@src/pages/Mod" },
    });

    app.use(mod);

    const spec = app[GET_TS_APP_SPEC]();
    expect(spec.pages.get("AppPage")).toBeDefined();
    expect(spec.pages.get("ModPage")).toBeDefined();
  });
});
