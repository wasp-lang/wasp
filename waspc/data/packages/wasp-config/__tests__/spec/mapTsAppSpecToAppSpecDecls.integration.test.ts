import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { mapTsAppSpecToAppSpecDecls } from "../../src/spec/mapTsAppSpecToAppSpecDecls.js";
import { app, page, query } from "../../src/spec/publicApi/index.js";

describe("mapTsAppSpecToAppSpecDecls", () => {
  test("maps an app with one page and one query to AppSpec.Decl[]", () => {
    const spec = app({
      name: "TodoApp",
      wasp: { version: "^0.16.3" },
      title: "Todo",
      parts: [
        page({ importDefault: "HomePage", from: "@src/Home" }),
        query(
          { import: "getTasks", from: "@src/operations" },
          { entities: ["Task"] },
        ),
      ],
    });

    const decls = mapTsAppSpecToAppSpecDecls(spec, ["Task"]);

    const expected: AppSpec.Decl[] = [
      {
        declType: "App",
        declName: "TodoApp",
        declValue: {
          wasp: { version: "^0.16.3" },
          title: "Todo",
          head: undefined,
          auth: undefined,
          server: undefined,
          client: undefined,
          db: undefined,
          emailSender: undefined,
          webSocket: undefined,
        },
      },
      {
        declType: "Page",
        declName: "HomePage",
        declValue: {
          component: { kind: "default", name: "HomePage", path: "@src/Home" },
          authRequired: undefined,
        },
      },
      {
        declType: "Query",
        declName: "getTasks",
        declValue: {
          fn: { kind: "named", name: "getTasks", path: "@src/operations" },
          entities: [{ name: "Task", declType: "Entity" }],
          auth: undefined,
        },
      },
    ];

    expect(decls).toEqual(expected);
  });

  // TODO: dedup pages referenced multiple times in parts → one Page decl.
  // TODO: two pages deriving the same name with different configs → throw.
  // TODO: duplicate query name → throw.
});
