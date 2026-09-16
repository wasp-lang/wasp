import { describe, expect, test } from "vitest";
import {
  RailwayCliDomainSchema,
  RailwayCliServiceStatusSchema,
} from "../../../src/providers/railway/jsonOutputSchemas.js";

describe("RailwayCliDomainSchema", () => {
  test("parses new format with domains array", () => {
    const input = {
      domains: ["my-app.up.railway.app", "custom.example.com"],
    };
    const result = RailwayCliDomainSchema.parse(input);
    expect(result).toEqual({
      domains: ["my-app.up.railway.app", "custom.example.com"],
    });
  });

  test("normalizes single-domain output", () => {
    const result = RailwayCliDomainSchema.parse({
      domain: "https://my-app.up.railway.app",
    });
    expect(result).toEqual({
      domains: ["https://my-app.up.railway.app"],
    });
  });

  test("rejects domains array that is empty", () => {
    expect(() => RailwayCliDomainSchema.parse({ domains: [] })).toThrow();
  });
});

describe("RailwayCliServiceStatusSchema", () => {
  test("treats a missing or unknown status as not ready", () => {
    expect(RailwayCliServiceStatusSchema.parse({}).status).toBeNull();
    expect(
      RailwayCliServiceStatusSchema.parse({ status: "BRAND_NEW_STATUS" })
        .status,
    ).toBeNull();
  });
});
