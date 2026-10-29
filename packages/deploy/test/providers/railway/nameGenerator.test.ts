import { describe, expect, test } from "vitest";
import type { RailwayProjectName } from "../../../src/providers/railway/brandedTypes.js";
import {
  createRailwayClientServiceName,
  createRailwayDbServiceName,
  createRailwayServerServiceName,
} from "../../../src/providers/railway/railwayService/nameGenerator.js";

describe("createRailwayClientServiceName", () => {
  test("appends -client suffix", () => {
    const name = createRailwayClientServiceName("my-app" as RailwayProjectName);
    expect(name).toBe("my-app-client");
  });
});

describe("createRailwayServerServiceName", () => {
  test("appends -server suffix", () => {
    const name = createRailwayServerServiceName("my-app" as RailwayProjectName);
    expect(name).toBe("my-app-server");
  });
});

describe("createRailwayDbServiceName", () => {
  test("always returns Postgres", () => {
    expect(createRailwayDbServiceName()).toBe("Postgres");
  });
});
