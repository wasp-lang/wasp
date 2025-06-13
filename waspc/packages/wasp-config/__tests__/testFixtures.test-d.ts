import { describe, expectTypeOf, test } from "vitest";
import { Branded } from "../src/branded";
import { EmptyObject, FullConfig, MinimalConfig } from "./testFixtures";

describe("MinimalConfig<T>", () => {
  test("should handle primitive types", async () => {
    expectTypeOf<MinimalConfig<string>>().toEqualTypeOf<string>();
    expectTypeOf<MinimalConfig<number>>().toEqualTypeOf<number>();
    expectTypeOf<MinimalConfig<boolean>>().toEqualTypeOf<boolean>();
    expectTypeOf<MinimalConfig<null>>().toEqualTypeOf<null>();
    expectTypeOf<MinimalConfig<undefined>>().toEqualTypeOf<undefined>();
    expectTypeOf<MinimalConfig<bigint>>().toEqualTypeOf<bigint>();
    expectTypeOf<MinimalConfig<symbol>>().toEqualTypeOf<symbol>();
  });

  test("should handle empty object", async () => {
    expectTypeOf<
      MinimalConfig<NoPropertiesObject>
    >().toEqualTypeOf<EmptyObject>();
  });

  test("should handle optional props object", async () => {
    expectTypeOf<MinimalConfig<OptionalObject>>().toEqualTypeOf<EmptyObject>();
  });

  test("should handle nested optional props object", async () => {
    expectTypeOf<
      MinimalConfig<OptionalNested<OptionalObject>>
    >().toEqualTypeOf<EmptyObject>();
  });

  test("should handle required props object", async () => {
    expectTypeOf<
      MinimalConfig<RequiredObject>
    >().toEqualTypeOf<RequiredObject>();
  });

  test("should handle nested required props object", async () => {
    expectTypeOf<MinimalConfig<RequiredNested<RequiredObject>>>().toEqualTypeOf<
      RequiredNested<RequiredObject>
    >();
  });

  test("should handle nested optional props object", async () => {
    expectTypeOf<MinimalConfig<RequiredNested<OptionalObject>>>().toEqualTypeOf<
      RequiredNested<EmptyObject>
    >();
  });

  test("should handle array with optional props object", async () => {
    expectTypeOf<MinimalConfig<OptionalObject[]>>().toEqualTypeOf<
      EmptyObject[]
    >();
  });

  test("should handle array with required props object", async () => {
    expectTypeOf<MinimalConfig<RequiredObject[]>>().toEqualTypeOf<
      RequiredObject[]
    >();
  });

  test("should handle branded type", async () => {
    expectTypeOf<MinimalConfig<BrandedType>>().toEqualTypeOf<BrandedType>();
  });
});

describe("FullConfig<T>", () => {
  test("should handle primitive types", async () => {
    expectTypeOf<FullConfig<string>>().toEqualTypeOf<string>();
    expectTypeOf<FullConfig<number>>().toEqualTypeOf<number>();
    expectTypeOf<FullConfig<boolean>>().toEqualTypeOf<boolean>();
    expectTypeOf<FullConfig<null>>().toEqualTypeOf<null>();
    expectTypeOf<FullConfig<undefined>>().toEqualTypeOf<undefined>();
    expectTypeOf<FullConfig<bigint>>().toEqualTypeOf<bigint>();
    expectTypeOf<FullConfig<symbol>>().toEqualTypeOf<symbol>();
  });

  test("should handle empty object", async () => {
    expectTypeOf<
      FullConfig<NoPropertiesObject>
    >().toEqualTypeOf<NoPropertiesObject>();
  });

  test("should handle optional props object", async () => {
    expectTypeOf<FullConfig<OptionalObject>>().toEqualTypeOf<RequiredObject>();
  });

  test("should handle nested optional props object", async () => {
    expectTypeOf<FullConfig<OptionalNested<OptionalObject>>>().toEqualTypeOf<
      RequiredNested<RequiredObject>
    >();
  });

  test("should handle required props object", async () => {
    expectTypeOf<FullConfig<RequiredObject>>().toEqualTypeOf<RequiredObject>();
  });

  test("should handle nested required props object", async () => {
    expectTypeOf<FullConfig<RequiredNested<RequiredObject>>>().toEqualTypeOf<
      RequiredNested<RequiredObject>
    >();
  });

  test("should handle nested required props object", async () => {
    expectTypeOf<FullConfig<RequiredNested<RequiredObject>>>().toEqualTypeOf<
      RequiredNested<RequiredObject>
    >();
  });

  test("should handle nested optional props object", async () => {
    expectTypeOf<FullConfig<RequiredNested<OptionalObject>>>().toEqualTypeOf<
      RequiredNested<RequiredObject>
    >();
  });

  test("should handle array with optional props object", async () => {
    expectTypeOf<FullConfig<OptionalObject[]>>().toEqualTypeOf<
      RequiredObject[]
    >();
  });

  test("should handle array with required props object", async () => {
    expectTypeOf<FullConfig<RequiredObject[]>>().toEqualTypeOf<
      RequiredObject[]
    >();
  });

  test("should handle branded type", async () => {
    expectTypeOf<FullConfig<BrandedType>>().toEqualTypeOf<BrandedType>();
  });
});

// eslint-disable-next-line @typescript-eslint/no-empty-object-type
type NoPropertiesObject = {};

type OptionalObject = {
  prop?: string;
  anotherProp?: number;
};

type RequiredObject = Required<OptionalObject>;

type OptionalNested<T> = {
  nested?: T;
};

type RequiredNested<T> = Required<OptionalNested<T>>;

type BrandedType = Branded<string, "BrandedString">;
