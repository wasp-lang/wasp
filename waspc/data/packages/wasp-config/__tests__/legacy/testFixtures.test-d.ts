import { describe, expectTypeOf, test } from "vitest";
import { Branded } from "../../src/branded.js";
import * as TsAppSpec from "../../src/legacy/publicApi/tsAppSpec.js";
import { FullConfig, MinimalConfig } from "./testFixtures.js";

describe("MinimalConfig<T>", () => {
  test("should not affect primitive types", async () => {
    expectTypeOf<MinimalConfig<string>>().toEqualTypeOf<string>();
    expectTypeOf<MinimalConfig<number>>().toEqualTypeOf<number>();
    expectTypeOf<MinimalConfig<boolean>>().toEqualTypeOf<boolean>();
    expectTypeOf<MinimalConfig<null>>().toEqualTypeOf<null>();
    expectTypeOf<MinimalConfig<undefined>>().toEqualTypeOf<undefined>();
    expectTypeOf<MinimalConfig<bigint>>().toEqualTypeOf<bigint>();
    expectTypeOf<MinimalConfig<symbol>>().toEqualTypeOf<symbol>();
  });

  test("should not affect branded types", async () => {
    expectTypeOf<MinimalConfig<BrandedType>>().toEqualTypeOf<BrandedType>();
  });

  test("should not affect function types", async () => {
    expectTypeOf<MinimalConfig<FunctionType>>().toEqualTypeOf<FunctionType>();
  });

  test("should convert no props object to EmptyObject", async () => {
    // eslint-disable-next-line @typescript-eslint/no-empty-object-type
    expectTypeOf<MinimalConfig<{}>>().toEqualTypeOf<EmptyObject>();
  });

  test("should remove optional props", async () => {
    expectTypeOf<
      MinimalConfig<ObjectWithOptionalPrimitiveProperties>
    >().toEqualTypeOf<EmptyObject>();
  });

  test("should not affect required props", async () => {
    expectTypeOf<
      MinimalConfig<ObjectWithRequiredPrimitiveProperties>
    >().toEqualTypeOf<ObjectWithRequiredPrimitiveProperties>();
  });

  test("should recursively apply to nested objects", async () => {
    expectTypeOf<
      MinimalConfig<{ nested1: { nested2: unknown } }>
    >().toEqualTypeOf<{
      nested1: { nested2: MinimalConfig<unknown> };
    }>();
  });

  test("Should recursively apply to array items", async () => {
    expectTypeOf<MinimalConfig<unknown[]>>().toEqualTypeOf<
      MinimalConfig<unknown>[]
    >();
  });
});

describe("FullConfig<T>", () => {
  test("should not affect primitive types", async () => {
    expectTypeOf<FullConfig<string>>().toEqualTypeOf<string>();
    expectTypeOf<FullConfig<number>>().toEqualTypeOf<number>();
    expectTypeOf<FullConfig<boolean>>().toEqualTypeOf<boolean>();
    expectTypeOf<FullConfig<null>>().toEqualTypeOf<null>();
    expectTypeOf<FullConfig<undefined>>().toEqualTypeOf<undefined>();
    expectTypeOf<FullConfig<bigint>>().toEqualTypeOf<bigint>();
    expectTypeOf<FullConfig<symbol>>().toEqualTypeOf<symbol>();
  });

  test("should not affect branded types", async () => {
    expectTypeOf<FullConfig<BrandedType>>().toEqualTypeOf<BrandedType>();
  });

  test("should not affect function types", async () => {
    expectTypeOf<FullConfig<FunctionType>>().toEqualTypeOf<FunctionType>();
  });

  test("should not affect required props", async () => {
    expectTypeOf<
      FullConfig<ObjectWithRequiredPrimitiveProperties>
    >().toEqualTypeOf<ObjectWithRequiredPrimitiveProperties>();
  });

  test("should make optional props required", async () => {
    expectTypeOf<
      FullConfig<ObjectWithOptionalPrimitiveProperties>
    >().toEqualTypeOf<ObjectWithRequiredPrimitiveProperties>();
  });

  test("should recursively apply to nested objects", async () => {
    expectTypeOf<
      FullConfig<{ nested1: { nested2: unknown } }>
    >().toEqualTypeOf<{
      nested1: { nested2: FullConfig<unknown> };
    }>();
  });

  test("Should recursively apply to array items", async () => {
    expectTypeOf<FullConfig<unknown[]>>().toEqualTypeOf<
      FullConfig<unknown>[]
    >();
  });
});

describe("import-form authored value types", () => {
  test("should accept functions at callable ExtImport use sites", async () => {
    const component = () => null;
    const operation = async () => null;

    const pageConfig = { component } satisfies TsAppSpec.PageConfig;
    const actionConfig = { fn: operation } satisfies TsAppSpec.ActionConfig;

    expectTypeOf(
      pageConfig.component,
    ).toMatchTypeOf<TsAppSpec.FunctionExtImport>();
    expectTypeOf(actionConfig.fn).toMatchTypeOf<TsAppSpec.FunctionExtImport>();
  });

  test("should accept objects at object ExtImport use sites", async () => {
    const envValidationSchema = { parse: (env: unknown) => env };

    const serverConfig = {
      envValidationSchema,
    } satisfies TsAppSpec.ServerConfig;

    expectTypeOf(
      serverConfig.envValidationSchema,
    ).toMatchTypeOf<TsAppSpec.ObjectExtImport>();
  });

  test("should reject functions at object ExtImport use sites", async () => {
    const serverConfig: TsAppSpec.ServerConfig = {
      // @ts-expect-error object ExtImport use sites must not accept callables.
      envValidationSchema: () => null,
    };

    expectTypeOf(serverConfig).toMatchTypeOf<TsAppSpec.ServerConfig>();
  });

  test("should reject malformed descriptor-like objects", async () => {
    const serverConfig: TsAppSpec.ServerConfig = {
      // @ts-expect-error descriptor-like objects must still satisfy ExtImport.
      envValidationSchema: { from: 42, importDefault: "X" },
    };

    expectTypeOf(serverConfig).toMatchTypeOf<TsAppSpec.ServerConfig>();
  });
});

type ObjectWithRequiredPrimitiveProperties = {
  prop: unknown;
  anotherProp: unknown;
};

type ObjectWithOptionalPrimitiveProperties =
  Partial<ObjectWithRequiredPrimitiveProperties>;

type BrandedType = Branded<string, "BrandedString">;

type FunctionType = (value: string) => number;

/**
 * Represents an empty object type in TypeScript.
 * @see https://www.totaltypescript.com/the-empty-object-type-in-typescript
 */
type EmptyObject = Record<string, never>;
