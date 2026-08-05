import { describe, expect, test } from "vitest";
import { extractServiceUrlFromString } from "../../../src/providers/railway/railwayService/url.js";

describe("extractServiceUrlFromString", () => {
  test("extracts URL from Railway CLI output", () => {
    const output =
      "Domains already exist on the service: https://my-app.up.railway.app";
    expect(extractServiceUrlFromString(output)).toBe(
      "https://my-app.up.railway.app",
    );
  });

  test("extracts URL when surrounded by other text", () => {
    const output =
      "Some prefix text https://example.railway.app/path and more text";
    expect(extractServiceUrlFromString(output)).toBe(
      "https://example.railway.app/path",
    );
  });

  test("throws when no URL is found", () => {
    expect(() => extractServiceUrlFromString("no url here")).toThrow(
      "Failed to get service domain",
    );
  });

  test("extracts first URL when multiple are present", () => {
    const output = "https://first.railway.app https://second.railway.app";
    expect(extractServiceUrlFromString(output)).toBe(
      "https://first.railway.app",
    );
  });
});
