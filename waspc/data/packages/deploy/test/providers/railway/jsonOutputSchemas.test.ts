import { describe, expect, test } from "vitest";
import {
  RailwayCliDomainSchema,
  RailwayCliProjectSchema,
  RailwayCliServiceStatusSchema,
  RailwayProjectListSchema,
} from "../../../src/providers/railway/jsonOutputSchemas.js";
import {
  cliProjectWithServices,
  cliProjectWithoutServices,
} from "./fixtures/railwayCliProject.js";

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

describe("RailwayCliProjectSchema", () => {
  test("parses a project with services", () => {
    const result = RailwayCliProjectSchema.parse(cliProjectWithServices);
    expect(result.id).toBe(cliProjectWithServices.id);
    expect(result.name).toBe(cliProjectWithServices.name);
    expect(result.services.edges).toHaveLength(2);
  });

  test("parses a project with no services", () => {
    const result = RailwayCliProjectSchema.parse(cliProjectWithoutServices);
    expect(result.services.edges).toEqual([]);
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

describe("RailwayProjectListSchema", () => {
  test("parses a list of projects", () => {
    const input = [cliProjectWithServices, cliProjectWithoutServices];
    const result = RailwayProjectListSchema.parse(input);
    expect(result).toHaveLength(2);
  });

  test("parses empty list", () => {
    expect(RailwayProjectListSchema.parse([])).toEqual([]);
  });
});
