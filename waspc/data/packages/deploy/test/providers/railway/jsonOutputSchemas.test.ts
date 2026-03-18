import { describe, expect, test } from "vitest";
import {
  RailwayCliDomainSchema,
  RailwayCliProjectSchema,
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

  test("parses legacy format with single domain and normalizes", () => {
    const input = { domain: "my-app.up.railway.app" };
    const result = RailwayCliDomainSchema.parse(input);
    expect(result).toEqual({ domains: ["my-app.up.railway.app"] });
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
