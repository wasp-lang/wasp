import { describe, expect, it } from "vitest";
import { interpolatePath } from "../src/router/interpolatePath";

describe("interpolatePath", () => {
  it.each([
    ["/items/:id", { id: 0 }, "/items/0"],
    ["/items/:id/details", { id: 0 }, "/items/0/details"],
    ["/items/:id?", { id: 0 }, "/items/0"],
    ["/files/*", { "*": 0 }, "/files/0"],
    ["/items/:id", { id: 123 }, "/items/123"],
    ["/items/:id", { id: "0" }, "/items/0"],
    ["/items/:id?", {}, "/items"],
    ["/items/:id?/details", {}, "/items/details"],
    ["/items/:id?", { id: "" }, "/items"],
    ["/files/*", { "*": "a/b" }, "/files/a/b"],
  ])("interpolates %s with %j as %s", (path, params, expected) => {
    expect(interpolatePath(path, params)).toBe(expected);
  });

  it("preserves zero with search parameters and a hash", () => {
    expect(
      interpolatePath("/items/:id", { id: 0 }, { tab: "info" }, "top"),
    ).toBe("/items/0?tab=info#top");
  });
});
