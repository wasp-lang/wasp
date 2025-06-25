import { describe, expectTypeOf, test } from "vitest";
import { Branded } from "../src/branded.js";
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

  test("should convert no props object to EmptyObject", async () => {
    // eslint-disable-next-line @typescript-eslint/no-empty-object-type
    expectTypeOf<MinimalConfig<{}>>().toEqualTypeOf<EmptyObject>();
  });

  test("should remove optional props", async () => {
    expectTypeOf<MinimalConfig<OptionalObject>>().toEqualTypeOf<EmptyObject>();
  });

  test("should not affect required props", async () => {
    expectTypeOf<
      MinimalConfig<RequiredObject>
    >().toEqualTypeOf<RequiredObject>();
  });

  test("should handle objects recursively", async () => {
    expectTypeOf<MinimalConfig<{ nested: unknown }>>().toEqualTypeOf<{
      nested: MinimalConfig<unknown>;
    }>();
  });

  test("should handle arrays recursively", async () => {
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

  test("should not affect required props", async () => {
    expectTypeOf<FullConfig<RequiredObject>>().toEqualTypeOf<RequiredObject>();
  });

  test("should make optional props required", async () => {
    expectTypeOf<FullConfig<OptionalObject>>().toEqualTypeOf<RequiredObject>();
  });

  test("should handle objects recursively", async () => {
    expectTypeOf<FullConfig<{ nested: unknown }>>().toEqualTypeOf<{
      nested: FullConfig<unknown>;
    }>();
  });

  test("should handle arrays recursively", async () => {
    expectTypeOf<FullConfig<unknown[]>>().toEqualTypeOf<
      FullConfig<unknown>[]
    >();
  });
});

type RequiredObject = {
  prop: string;
  anotherProp: number;
};

type OptionalObject = Partial<RequiredObject>;

type BrandedType = Branded<string, "BrandedString">;

/**
 * Represents an empty object type in TypeScript.
 * @see https://www.totaltypescript.com/the-empty-object-type-in-typescript
 */
export type EmptyObject = Record<string, never>;
