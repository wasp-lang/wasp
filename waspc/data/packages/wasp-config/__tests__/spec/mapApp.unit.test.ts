import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import {
  deriveExtImportName,
  makeRefParser,
  mapAction,
  mapApp,
  mapClient,
  mapDb,
  mapEmailFromField,
  mapEmailSender,
  mapExtImport,
  mapJob,
  mapPage,
  mapQuery,
  mapRoute,
  mapSchedule,
  mapServer,
  mapWebSocket,
} from "../../src/spec/mapApp.js";
import { app, page, route } from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

describe("mapApp", () => {
  test("should map minimal app correctly", () => {
    const entityNames = Fixtures.getEntities("minimal");
    const app = Fixtures.getApp("minimal");

    const decls = mapApp(app, entityNames);

    expect(decls).toStrictEqual([
      {
        declType: "App",
        declName: app.name,
        declValue: {
          wasp: app.wasp,
          title: app.title,
          head: app.head,
          auth: undefined,
          server: undefined,
          client: undefined,
          db: undefined,
          emailSender: undefined,
          webSocket: undefined,
        },
      },
    ] satisfies AppSpec.Decl[]);
  });

  test("should map full app correctly", () => {
    // Note: we build the full app inline (instead of using `Fixtures.getApp("full")`)
    // so the assertions can name each part explicitly when computing expected decls.
    const page = Fixtures.getPage("full");
    const route = Fixtures.getRoute("full");
    const query = Fixtures.getQuery("full");
    const job = Fixtures.getJob("full");
    const server = Fixtures.getServerConfig("full");
    const client = Fixtures.getClientConfig("full");
    const db = Fixtures.getDbConfig("full");
    const emailSender = Fixtures.getEmailSenderConfig("full");
    const webSocket = Fixtures.getWebSocketConfig("full");
    const entityNames = Fixtures.getEntities("full");
    const entityRefParser = makeRefParser("Entity", entityNames);

    const inputApp = app({
      name: "FullApp",
      wasp: { version: "^0.16.3" },
      title: "Mock App",
      head: ['<link rel="icon" href="/favicon.ico" />'],
      server,
      client,
      db,
      emailSender,
      webSocket,
      parts: [page, route, query, job],
    });

    const result = mapApp(inputApp, entityNames);

    // TODO: Reaching into `deriveExtImportName` here is not ideal — it leaks
    // an orchestrator-internal helper into the test. Revisit once we have a
    // higher-level name-derivation system: either a part-agnostic
    // `deriveDeclName(part)`, or a part-specific dispatch (mirroring how we
    // have one mapper per kind). The test should delegate to whatever
    // surfaces.
    expect(result).toStrictEqual([
      {
        declType: "App",
        declName: inputApp.name,
        declValue: {
          wasp: inputApp.wasp,
          title: inputApp.title,
          head: inputApp.head,
          auth: undefined,
          server: mapServer(server),
          client: mapClient(client),
          db: mapDb(db),
          emailSender: mapEmailSender(emailSender),
          webSocket: mapWebSocket(webSocket),
        },
      },
      {
        declType: "Page",
        declName: deriveExtImportName(page.component),
        declValue: mapPage(page),
      },
      {
        declType: "Route",
        declName: route.name,
        declValue: mapRoute(route),
      },
      {
        declType: "Query",
        declName: deriveExtImportName(query.fn),
        declValue: mapQuery(query, entityRefParser),
      },
      {
        declType: "Job",
        declName: deriveExtImportName(job.fn),
        declValue: mapJob(job, entityRefParser),
      },
    ] satisfies AppSpec.Decl[]);
  });

  test("dedups a page referenced explicitly twice", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);

    const app = Fixtures.getMinimalAppWithParts([page1, page2]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced via a route shorthand twice", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([route1, route2]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced explicitly and via a route shorthand", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);
    const route1 = route("Route1", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([page1, route1]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("throws when the same page name is produced with differing configs explicitly", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });

    const app = Fixtures.getMinimalAppWithParts([page1, page2]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configs for page "${pageName}"`,
    );
  });

  test("throws when the same page name is produced with differing configs via a route shorthand twice", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([route1, route2]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configs for page "${pageName}"`,
    );
  });

  test("throws when the same page name is produced with differing configs explicitly and via a route shorthand", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });
    const route1 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([page1, route1]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configs for page "${pageName}"`,
    );
  });

  // TODO: duplicate query name → throw.
});

describe("mapPage", () => {
  test("should map minimal config correctly", () => {
    testMapPage(Fixtures.getPage("minimal"));
  });

  test("should map full config correctly", () => {
    testMapPage(Fixtures.getPage("full"));
  });

  function testMapPage(page: TsAppSpec.Page): void {
    const result = mapPage(page);

    expect(result).toStrictEqual({
      component: mapExtImport(page.component),
      authRequired: page.authRequired,
    } satisfies AppSpec.Page);
  }
});

describe("mapRoute", () => {
  test("should map minimal config correctly", () => {
    testMapRoute(Fixtures.getRoute("minimal"));
  });

  test("should map full config correctly", () => {
    testMapRoute(Fixtures.getRoute("full"));
  });

  function testMapRoute(route: TsAppSpec.Route): void {
    const result = mapRoute(route);

    expect(result).toStrictEqual({
      path: route.path,
      to: {
        name: deriveExtImportName(route.page.component),
        declType: "Page",
      },
      prerender: route.prerender,
      lazy: route.lazy,
    } satisfies AppSpec.Route);
  }
});

describe("mapQuery", () => {
  test("should map minimal config correctly", () => {
    testMapQuery(Fixtures.getQuery("minimal"));
  });

  test("should map full config correctly", () => {
    testMapQuery(Fixtures.getQuery("full"));
  });

  test("should throw if entity ref is not provided", () => {
    const query = Fixtures.getQuery("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapQuery(query, entityRefParser)).toThrowError();
  });

  function testMapQuery(query: TsAppSpec.Query): void {
    const entityRefParser = makeRefParser("Entity", query.entities ?? []);

    const result = mapQuery(query, entityRefParser);

    expect(result).toStrictEqual({
      fn: mapExtImport(query.fn),
      entities: query.entities?.map(entityRefParser),
      auth: query.auth,
    } satisfies AppSpec.Query);
  }
});

describe("mapAction", () => {
  test("should map minimal config correctly", () => {
    testMapAction(Fixtures.getAction("minimal"));
  });

  test("should map full config correctly", () => {
    testMapAction(Fixtures.getAction("full"));
  });

  test("should throw if entity ref is not provided", () => {
    const action = Fixtures.getAction("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapAction(action, entityRefParser)).toThrowError();
  });

  function testMapAction(action: TsAppSpec.Action): void {
    const entityRefParser = makeRefParser("Entity", action.entities ?? []);

    const result = mapAction(action, entityRefParser);

    expect(result).toStrictEqual({
      fn: mapExtImport(action.fn),
      entities: action.entities?.map(entityRefParser),
      auth: action.auth,
    } satisfies AppSpec.Action);
  }
});

describe("mapServer", () => {
  test("should map minimal config correctly", () => {
    testMapServer(Fixtures.getServerConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapServer(Fixtures.getServerConfig("full"));
  });

  function testMapServer(server: TsAppSpec.Server): void {
    const result = mapServer(server);

    expect(result).toStrictEqual({
      setupFn: server.setupFn && mapExtImport(server.setupFn),
      middlewareConfigFn:
        server.middlewareConfigFn && mapExtImport(server.middlewareConfigFn),
      envValidationSchema:
        server.envValidationSchema && mapExtImport(server.envValidationSchema),
    } satisfies AppSpec.Server);
  }
});

describe("mapClient", () => {
  test("should map minimal config correctly", () => {
    testMapClient(Fixtures.getClientConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapClient(Fixtures.getClientConfig("full"));
  });

  function testMapClient(client: TsAppSpec.Client): void {
    const result = mapClient(client);

    expect(result).toStrictEqual({
      rootComponent: client.rootComponent && mapExtImport(client.rootComponent),
      setupFn: client.setupFn && mapExtImport(client.setupFn),
      baseDir: client.baseDir,
      envValidationSchema:
        client.envValidationSchema && mapExtImport(client.envValidationSchema),
    } satisfies AppSpec.Client);
  }
});

describe("mapDb", () => {
  test("should map minimal config correctly", () => {
    testMapDb(Fixtures.getDbConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapDb(Fixtures.getDbConfig("full"));
  });

  function testMapDb(db: TsAppSpec.Db): void {
    const result = mapDb(db);

    expect(result).toStrictEqual({
      seeds: db.seeds?.map((seed) => mapExtImport(seed)),
      prismaSetupFn: db.prismaSetupFn && mapExtImport(db.prismaSetupFn),
    } satisfies AppSpec.Db);
  }
});

describe("mapEmailSender", () => {
  test("should map minimal config correctly", () => {
    testMapEmailSender(Fixtures.getEmailSenderConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapEmailSender(Fixtures.getEmailSenderConfig("full"));
  });

  function testMapEmailSender(emailSender: TsAppSpec.EmailSender): void {
    const result = mapEmailSender(emailSender);

    expect(result).toStrictEqual({
      provider: emailSender.provider,
      defaultFrom:
        emailSender.defaultFrom && mapEmailFromField(emailSender.defaultFrom),
    } satisfies AppSpec.EmailSender);
  }
});

describe("mapEmailFromField", () => {
  test("should map minimal config correctly", () => {
    testMapEmailFromField(Fixtures.getEmailFromField("minimal"));
  });

  test("should map full config correctly", () => {
    testMapEmailFromField(Fixtures.getEmailFromField("full"));
  });

  function testMapEmailFromField(
    emailFromField: TsAppSpec.EmailFromField,
  ): void {
    const result = mapEmailFromField(emailFromField);

    expect(result).toStrictEqual({
      name: emailFromField.name,
      email: emailFromField.email,
    } satisfies AppSpec.EmailFromField);
  }
});

describe("mapWebSocket", () => {
  test("should map minimal config correctly", () => {
    testMapWebSocket(Fixtures.getWebSocketConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapWebSocket(Fixtures.getWebSocketConfig("full"));
  });

  function testMapWebSocket(webSocket: TsAppSpec.WebSocket): void {
    const result = mapWebSocket(webSocket);

    expect(result).toStrictEqual({
      fn: mapExtImport(webSocket.fn),
      autoConnect: webSocket.autoConnect,
    } satisfies AppSpec.WebSocket);
  }
});

describe("mapJob", () => {
  test("should map minimal config correctly", () => {
    testMapJob(Fixtures.getJob("minimal"));
  });

  test("should map full config correctly", () => {
    testMapJob(Fixtures.getJob("full"));
  });

  test("should throw if entity ref is not provided", () => {
    const job = Fixtures.getJob("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapJob(job, entityRefParser)).toThrowError();
  });

  function testMapJob(job: TsAppSpec.Job): void {
    const entityRefParser = makeRefParser("Entity", job.entities ?? []);

    const result = mapJob(job, entityRefParser);

    expect(result).toStrictEqual({
      executor: job.executor,
      perform: {
        fn: mapExtImport(job.fn),
        executorOptions: job.performExecutorOptions,
      },
      schedule: job.schedule && mapSchedule(job.schedule),
      entities: job.entities?.map(entityRefParser),
    } satisfies AppSpec.Job);
  }
});

describe("mapSchedule", () => {
  test("should map minimal config correctly", () => {
    testMapSchedule(Fixtures.getSchedule("minimal"));
  });

  test("should map full config correctly", () => {
    testMapSchedule(Fixtures.getSchedule("full"));
  });

  function testMapSchedule(schedule: TsAppSpec.Schedule): void {
    const result = mapSchedule(schedule);

    expect(result).toStrictEqual({
      cron: schedule.cron,
      args: schedule.args,
      executorOptions: schedule.executorOptions,
    } satisfies AppSpec.Schedule);
  }
});

describe("mapExtImport", () => {
  test("should map minimal named import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("minimal", "named"));
  });

  test("should map full named import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("full", "named"));
  });

  test("should map minimal default import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("minimal", "default"));
  });

  test("should map full default import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("full", "default"));
  });

  function testMapExtImport(extImport: TsAppSpec.ExtImport): void {
    const result = mapExtImport(extImport);

    if ("import" in extImport) {
      expect(result).toStrictEqual({
        kind: "named",
        name: extImport.import,
        path: extImport.from,
      } satisfies AppSpec.ExtImport);
    } else {
      expect(result).toStrictEqual({
        kind: "default",
        name: extImport.importDefault,
        path: extImport.from,
      } satisfies AppSpec.ExtImport);
    }
  }
});
