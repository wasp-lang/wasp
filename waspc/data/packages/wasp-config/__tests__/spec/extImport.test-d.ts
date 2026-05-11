import { describe, expectTypeOf, test } from "vitest";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

describe("ExtImport input types", () => {
  test("should accept functions at callable ExtImport use sites", async () => {
    const component = () => null;
    const operation = async () => null;

    const pageConfig = { kind: "page", component } satisfies TsAppSpec.Page;
    const actionConfig = {
      kind: "action",
      fn: operation,
    } satisfies TsAppSpec.Action;

    expectTypeOf(
      pageConfig.component,
    ).toExtend<TsAppSpec.CallableExtImportInput>();
    expectTypeOf(actionConfig.fn).toExtend<TsAppSpec.CallableExtImportInput>();
  });

  test("should reject objects at callable ExtImport use sites", async () => {
    const component = { render: () => null };

    const pageConfig: TsAppSpec.Page = {
      kind: "page",
      // @ts-expect-error callable ExtImport sites accept descriptors or functions.
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
