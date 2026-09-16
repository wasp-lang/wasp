import { describe, expect, test } from "vitest";
import { normalizePrerender } from "../../src/normalizePrerender.js";

describe("normalizePrerender", () => {
  test("returns an empty list when prerendering is disabled", () => {
    expect(normalizePrerender(undefined, "/foo")).toEqual([]);
    expect(normalizePrerender(false, "/foo")).toEqual([]);
    expect(normalizePrerender([], "/foo")).toEqual([]);
  });

  test("expands `true` to the route's own path", () => {
    expect(normalizePrerender(true, "/foo")).toEqual(["/foo"]);
  });

  test("keeps an explicit list of paths as-is", () => {
    expect(normalizePrerender(["/a", "/b"], "/foo")).toEqual(["/a", "/b"]);
  });
});
