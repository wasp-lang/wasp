import { describe, expect, test } from "vitest";
import { ProcessOutput } from "zx";
import { isRailwayTransientError } from "../../../src/providers/railway/retry.js";

function makeProcessOutput(stderr: string): ProcessOutput {
  return new ProcessOutput(1, null, "", stderr, stderr, stderr);
}

describe("isRailwayTransientError", () => {
  describe("returns true for transient errors", () => {
    test("API timeout", () => {
      const error = makeProcessOutput(
        "Failed to fetch: error sending request for url (https://backboard.railway.com/graphql/v2)\n\nCaused by:\n    0: error sending request\n    1: operation timed out",
      );
      expect(isRailwayTransientError(error)).toBe(true);
    });

    test("connection refused", () => {
      const error = makeProcessOutput(
        "Failed to fetch: error sending request\n\nCaused by:\n    connection refused",
      );
      expect(isRailwayTransientError(error)).toBe(true);
    });

    test("DNS error", () => {
      const error = makeProcessOutput(
        "Failed to fetch: error sending request\n\nCaused by:\n    dns error: failed to lookup address",
      );
      expect(isRailwayTransientError(error)).toBe(true);
    });

    test("rate limited", () => {
      const error = makeProcessOutput(
        "You are being ratelimited. Please try again later",
      );
      expect(isRailwayTransientError(error)).toBe(true);
    });
  });

  describe("returns false for permanent errors", () => {
    test("unauthorized", () => {
      const error = makeProcessOutput(
        "Unauthorized. Please login with `railway login`",
      );
      expect(isRailwayTransientError(error)).toBe(false);
    });

    test("project not found", () => {
      const error = makeProcessOutput(
        "Project not found. Run `railway link` to connect to a project.",
      );
      expect(isRailwayTransientError(error)).toBe(false);
    });

    test("deploy failed", () => {
      const error = makeProcessOutput("Deploy failed");
      expect(isRailwayTransientError(error)).toBe(false);
    });

    test("file too large", () => {
      const error = makeProcessOutput(
        "Failed to upload code. File too large (524288000 bytes)",
      );
      expect(isRailwayTransientError(error)).toBe(false);
    });

    test("fetch error without transient cause", () => {
      const error = makeProcessOutput("Failed to fetch: some unknown error");
      expect(isRailwayTransientError(error)).toBe(false);
    });
  });

  describe("returns false for non-ProcessOutput errors", () => {
    test("plain Error", () => {
      expect(isRailwayTransientError(new Error("timed out"))).toBe(false);
    });

    test("string", () => {
      expect(isRailwayTransientError("Failed to fetch: timed out")).toBe(false);
    });
  });
});
