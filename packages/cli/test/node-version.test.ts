import { describe, expect, test } from "vitest";
import { CLIError } from "../src/cli-error.js";
import { assertNodeVersionIsSupported } from "../src/node-version.js";

describe("assertNodeVersionIsSupported", () => {
  test.each(["26.13.1", "26.13.2", "26.14.0", "27.0.0"])(
    "passes for Node.js %s when the range asks for >=26.13.1",
    (nodeVersion) => {
      expect(() =>
        assertNodeVersionIsSupported(nodeVersion, ">=26.13.1"),
      ).not.toThrow();
    },
  );

  test.each(["26.13.0", "26.2.9", "24.99.99"])(
    "fails for Node.js %s when the range asks for >=26.13.1",
    (nodeVersion) => {
      expect(() =>
        assertNodeVersionIsSupported(nodeVersion, ">=26.13.1"),
      ).toThrow(CLIError);
    },
  );

  test("names both versions in the error", () => {
    expect(() => assertNodeVersionIsSupported("24.22.1", ">=26.13.1")).toThrow(
      "Your Node.js version (24.22.1) is not supported. Please upgrade to Node.js 26.13.1 or later to use Wasp.",
    );
  });

  test("passes when the range has no full version to compare with", () => {
    expect(() => assertNodeVersionIsSupported("24.22.1", ">=26")).not.toThrow();
  });
});
