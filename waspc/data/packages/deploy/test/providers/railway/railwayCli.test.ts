import { describe, expect, test } from "vitest";
import type { RailwayProjectName } from "../../../src/providers/railway/brandedTypes.js";
import {
  assertRailwayProjectNameIsPresent,
  assertRailwayProjectNameIsValid,
} from "../../../src/providers/railway/railwayCli.js";

describe("assertRailwayProjectNameIsPresent", () => {
  test("throws when project name is undefined", () => {
    expect(() =>
      assertRailwayProjectNameIsPresent(undefined, "launch"),
    ).toThrow("Missing required argument: project name.");
  });

  test("includes usage hint with command name in error message", () => {
    expect(() =>
      assertRailwayProjectNameIsPresent(undefined, "launch"),
    ).toThrow("Usage: wasp deploy railway launch <project-name>");
  });

  test("includes correct command name for setup", () => {
    expect(() =>
      assertRailwayProjectNameIsPresent(undefined, "setup"),
    ).toThrow("Usage: wasp deploy railway setup <project-name>");
  });

  test("includes correct command name for deploy", () => {
    expect(() =>
      assertRailwayProjectNameIsPresent(undefined, "deploy"),
    ).toThrow("Usage: wasp deploy railway deploy <project-name>");
  });

  test("does not throw when project name is provided", () => {
    expect(() =>
      assertRailwayProjectNameIsPresent(
        "my-project" as RailwayProjectName,
        "launch",
      ),
    ).not.toThrow();
  });
});

describe("assertRailwayProjectNameIsValid", () => {
  test("does not throw for a short project name", () => {
    expect(() =>
      assertRailwayProjectNameIsValid("my-app" as RailwayProjectName),
    ).not.toThrow();
  });

  test("throws for a project name that is too long", () => {
    const longName = "a".repeat(100) as RailwayProjectName;
    expect(() => assertRailwayProjectNameIsValid(longName)).toThrow(
      "too long",
    );
  });
});
