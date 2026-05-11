import { describe, expectTypeOf, test } from "vitest";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import type { AnyFunction } from "../../src/typeUtils.js";

describe("ExtImport input types", () => {
  test("should accept functions at ExtImport use sites", async () => {
    const component = () => null;
    const operation = async (_args: { id: string }) => null;

    const pageConfig = { kind: "page", component } satisfies TsAppSpec.Page;
    const actionConfig = {
      kind: "action",
      fn: operation,
    } satisfies TsAppSpec.Action;

    expectTypeOf(pageConfig.component).toExtend<
      TsAppSpec.ExtImport | AnyFunction
    >();
    expectTypeOf(actionConfig.fn).toExtend<TsAppSpec.ExtImport | AnyFunction>();
  });

  test("should reject objects at ExtImport use sites", async () => {
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
