import { matchRoutes } from "react-router";
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
    ["/items/:id", { id: "a/b" }, "/items/a%2Fb"],
    ["/items/:id", { id: "a?b#c" }, "/items/a%3Fb%23c"],
    ["/items/:id", { id: "a b%" }, "/items/a%20b%25"],
    ["/files/*", { "*": "a b/c?d" }, "/files/a%20b/c%3Fd"],
  ])("interpolates %s with %j as %s", (path, params, expected) => {
    expect(interpolatePath(path, params)).toBe(expected);
  });

  // Wasp names this param `name.xml`, while React Router names it `name`.
  // See https://github.com/wasp-lang/wasp/issues/5454
  it("builds a matching path for a param with a literal suffix", () => {
    const path = interpolatePath("/feed/:name.xml", {
      "name.xml": "release.xml",
    });

    expect(path).toBe("/feed/release.xml");
    expect(matchRoutes([{ path: "/feed/:name.xml" }], path)).not.toBeNull();
  });

  it("preserves zero with search parameters and a hash", () => {
    expect(
      interpolatePath("/items/:id", { id: 0 }, { tab: "info" }, "top"),
    ).toBe("/items/0?tab=info#top");
  });
});
