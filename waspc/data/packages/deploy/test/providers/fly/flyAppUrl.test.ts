import { describe, expect, test } from "vitest";
import { getFlyAppUrl } from "../../../src/providers/fly/flyAppUrl.js";

describe("getFlyAppUrl", () => {
  test("returns the correct Fly.io app URL", () => {
    const flyAppName = "my-app";
    expect(getFlyAppUrl(flyAppName)).toBe(`https://${flyAppName}.fly.dev`);
  });
});
