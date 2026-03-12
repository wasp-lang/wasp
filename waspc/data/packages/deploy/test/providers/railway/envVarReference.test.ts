import { describe, expect, test } from "vitest";
import { getRailwayEnvVarValueReference } from "../../../src/providers/railway/envVarReference.js";

describe("getRailwayEnvVarValueReference", () => {
  test("creates a local variable reference", () => {
    expect(getRailwayEnvVarValueReference("PORT")).toBe("${{PORT}}");
  });

  test("creates a cross-service variable reference", () => {
    expect(
      getRailwayEnvVarValueReference("DATABASE_URL", {
        serviceName: "Postgres",
      }),
    ).toBe('${{"Postgres".DATABASE_URL}}');
  });

  test("wraps service name in quotes", () => {
    expect(
      getRailwayEnvVarValueReference("DATABASE_URL", {
        serviceName: "my-app-1",
      }),
    ).toBe('${{"my-app-1".DATABASE_URL}}');
  });

  test("handles special function syntax", () => {
    expect(getRailwayEnvVarValueReference("secret()")).toBe("${{secret()}}");
  });
});
