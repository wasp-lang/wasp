import { describe, expect, it } from "vitest";
import { DomainError } from "./errors";
import {
  principalFromMcp,
  principalFromSession,
  requireScope,
  requireSession,
} from "./principal";
import { ALL_SCOPES } from "./scopes";

const mcpCtx = (scopes: string[]) => ({
  user: { id: "u1" },
  scopes,
  connection: { clientName: "Claude" },
});

describe("principals", () => {
  it("gives dashboard sessions every scope", () => {
    const p = principalFromSession({ id: "u1" });
    expect([...p.scopes].sort()).toEqual([...ALL_SCOPES].sort());
    expect(p.source).toBe("session");
  });

  it("takes MCP permissions from the connection, ignoring unknown scopes", () => {
    const p = principalFromMcp(mcpCtx(["analytics:read", "nope"]));
    expect([...p.scopes]).toEqual(["analytics:read"]);
    expect(p.clientName).toBe("Claude");
  });
});

describe("requireScope", () => {
  it("passes when the scope is present", () => {
    expect(() =>
      requireScope(principalFromSession({ id: "u1" }), "links:write"),
    ).not.toThrow();
  });

  it("throws forbidden naming the missing scope", () => {
    try {
      requireScope(principalFromMcp(mcpCtx(["links:read"])), "links:write");
      expect.unreachable();
    } catch (e) {
      expect(e).toBeInstanceOf(DomainError);
      expect((e as DomainError).code).toBe("forbidden");
      expect((e as DomainError).message).toContain("links:write");
    }
  });
});

describe("requireSession", () => {
  it("rejects MCP connections", () => {
    expect(() =>
      requireSession(principalFromMcp(mcpCtx([])), "Approving an app"),
    ).toThrow(DomainError);
  });
});
