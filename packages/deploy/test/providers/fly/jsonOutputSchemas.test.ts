import { describe, expect, test } from "vitest";
import {
  FlyRegionListSchema,
  FlySecretListSchema,
} from "../../../src/providers/fly/jsonOutputSchemas.js";

describe("FlyRegionListSchema", () => {
  test("parses legacy format with uppercase keys", () => {
    const input = [
      { Code: "iad", Name: "Ashburn, Virginia (US)" },
      { Code: "lhr", Name: "London, United Kingdom" },
    ];
    const result = FlyRegionListSchema.parse(input);
    expect(result).toEqual([
      { code: "iad", name: "Ashburn, Virginia (US)" },
      { code: "lhr", name: "London, United Kingdom" },
    ]);
  });

  test("parses current format with lowercase keys", () => {
    const input = [
      { code: "iad", name: "Ashburn, Virginia (US)" },
      { code: "lhr", name: "London, United Kingdom" },
    ];
    const result = FlyRegionListSchema.parse(input);
    expect(result).toEqual(input);
  });

  test("parses wrapped format with Regions key", () => {
    const input = {
      Regions: [
        { code: "iad", name: "Ashburn, Virginia (US)" },
        { code: "lhr", name: "London, United Kingdom" },
      ],
    };
    const result = FlyRegionListSchema.parse(input);
    expect(result).toEqual(input.Regions);
  });

  test("parses empty array", () => {
    expect(FlyRegionListSchema.parse([])).toEqual([]);
  });
});

describe("FlySecretListSchema", () => {
  test("parses legacy format with uppercase Name and normalizes to lowercase", () => {
    const input = [{ Name: "DATABASE_URL" }, { Name: "SECRET_KEY" }];
    const result = FlySecretListSchema.parse(input);
    expect(result).toEqual([{ name: "DATABASE_URL" }, { name: "SECRET_KEY" }]);
  });

  test("parses current format with lowercase name", () => {
    const input = [{ name: "DATABASE_URL" }, { name: "SECRET_KEY" }];
    const result = FlySecretListSchema.parse(input);
    expect(result).toEqual(input);
  });

  test("parses empty array", () => {
    expect(FlySecretListSchema.parse([])).toEqual([]);
  });
});
