import { describe, expect, test } from "vitest";
import {
  RailwayCliDomainSchema,
  RailwayCliProjectSchema,
  RailwayCliProjectStatusSchema,
  RailwayProjectListSchema,
} from "../../../src/providers/railway/jsonOutputSchemas.js";
import {
  cliProjectWithServices,
  cliProjectWithoutServices,
} from "./fixtures/railwayCliProject.js";
import {
  cliProjectStatusInNewFormat,
  cliProjectStatusInOldFormat,
} from "./fixtures/railwayCliProjectStatus.js";

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

describe("RailwayCliProjectStatusSchema", () => {
  test("parses new CLI output with instances under environments", () => {
    const result = RailwayCliProjectStatusSchema.parse(
      cliProjectStatusInNewFormat,
    );
    expect(result.environments.edges).toHaveLength(1);
  });

  test("converts old CLI output with instances under services to the new format", () => {
    const result = RailwayCliProjectStatusSchema.parse(
      cliProjectStatusInOldFormat,
    );
    expect(result.environments.edges).toHaveLength(2);
    expect(result).not.toHaveProperty("services");
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
