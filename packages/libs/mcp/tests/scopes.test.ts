import { describe, expect, it } from "vitest";
import {
  capScopes,
  expandScopes,
  normalizeScopes,
  toggleScope,
} from "../src/scopes";

const SCOPES = {
  "links:read": { label: "View links" },
  "links:write": { label: "Edit links", implies: ["links:read"] },
  "links:admin": { label: "Administer links", implies: ["links:write"] },
  "analytics:read": { label: "View analytics" },
};

describe("scopes", () => {
  it("normalizes to declared scopes in declaration order", () => {
    expect(
      normalizeScopes(SCOPES, [
        "analytics:read",
        "nope",
        "links:read",
        "links:read",
      ]),
    ).toEqual(["links:read", "analytics:read"]);
  });

  it("expands implied scopes transitively", () => {
    expect(expandScopes(SCOPES, ["links:admin"])).toEqual([
      "links:read",
      "links:write",
      "links:admin",
    ]);
  });

  it("ticking a scope ticks what it implies; unticking unticks what implies it", () => {
    expect(toggleScope(SCOPES, [], "links:write", true)).toEqual([
      "links:read",
      "links:write",
    ]);
    expect(
      toggleScope(
        SCOPES,
        ["links:read", "links:write", "links:admin"],
        "links:read",
        false,
      ),
    ).toEqual([]);
    expect(
      toggleScope(
        SCOPES,
        ["links:read", "links:write", "analytics:read"],
        "links:write",
        false,
      ),
    ).toEqual(["links:read", "analytics:read"]);
  });

  it("caps a choice to what was requested", () => {
    expect(
      capScopes(SCOPES, ["links:write", "analytics:read"], ["links:write"]),
    ).toEqual(["links:read", "links:write"]);
  });
});
