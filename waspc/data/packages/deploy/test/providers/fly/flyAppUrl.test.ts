import { describe, expect, test } from "vitest";
import { getFlyAppUrl } from "../../../src/providers/fly/flyAppUrl.js";

describe("getFlyAppUrl", () => {
  test("returns the correct Fly.io app URL", () => {
    expect(getFlyAppUrl("my-app")).toBe("https://my-app.fly.dev");
  });
});
