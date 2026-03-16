import { describe, expect, test } from "vitest";
import {
  RailwayCliDomainSchema,
  RailwayCliProjectSchema,
  RailwayProjectListSchema,
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
    const input = {
      id: "proj-1",
      name: "my-project",
      services: {
        edges: [
          { node: { id: "svc-1", name: "web" } },
          { node: { id: "svc-2", name: "db" } },
        ],
      },
    };
    const result = RailwayCliProjectSchema.parse(input);
    expect(result.id).toBe(input.id);
    expect(result.name).toBe(input.name);
    expect(result.services.edges).toHaveLength(2);
  });

  test("parses a project with no services", () => {
    const input = {
      id: "proj-1",
      name: "my-project",
      services: { edges: [] },
    };
    const result = RailwayCliProjectSchema.parse(input);
    expect(result.services.edges).toEqual([]);
  });
});

describe("RailwayProjectListSchema", () => {
  test("parses a list of projects", () => {
    const input = [
      {
        id: "proj-1",
        name: "project-a",
        services: { edges: [] },
      },
      {
        id: "proj-2",
        name: "project-b",
        services: {
          edges: [{ node: { id: "svc-1", name: "web" } }],
        },
      },
    ];
    const result = RailwayProjectListSchema.parse(input);
    expect(result).toHaveLength(2);
  });

  test("parses empty list", () => {
    expect(RailwayProjectListSchema.parse([])).toEqual([]);
  });
});
