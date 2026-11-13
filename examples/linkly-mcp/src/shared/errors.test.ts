import { describe, expect, it, vi } from "vitest";
import { z } from "zod";
import { DomainError, toErrorResponse, toHttpResponse } from "./errors";

describe("toErrorResponse", () => {
  it.each([
    ["invalid", 400],
    ["unauthorized", 401],
    ["forbidden", 403],
    ["not_found", 404],
    ["conflict", 409],
  ] as const)("maps %s to %i", (code, status) => {
    const r = toErrorResponse(new DomainError(code, "nope"));
    expect(r.status).toBe(status);
    expect(r.body).toEqual({ error: { code, message: "nope" } });
  });

  it("turns Zod errors into readable `invalid` errors", () => {
    const parsed = z
      .object({ slug: z.string().min(3) })
      .safeParse({ slug: "a" });
    const r = toErrorResponse(parsed.error);
    expect(r.status).toBe(400);
    expect(r.body.error.code).toBe("invalid");
    expect(r.body.error.message).toContain("slug");
  });

  it("hides unknown errors behind a generic 500", () => {
    const r = toErrorResponse(new Error("database password is hunter2"));
    expect(r.status).toBe(500);
    expect(r.body.error.message).not.toContain("hunter2");
  });
});

describe("toHttpResponse", () => {
  it("writes status and JSON body", () => {
    const res = { status: vi.fn(), json: vi.fn() };
    res.status.mockReturnValue(res);
    toHttpResponse(new DomainError("not_found", 'Link "abc" not found.'), res);
    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({
      error: { code: "not_found", message: 'Link "abc" not found.' },
    });
  });
});

describe("DomainError", () => {
  it("exposes an HTTP status, so @wasp/lib-mcp maps it to a tool error", () => {
    const e = new DomainError("conflict", "Taken.");
    expect(e.statusCode).toBe(409);
    expect(e.data).toEqual({ code: "conflict" });
  });
});
