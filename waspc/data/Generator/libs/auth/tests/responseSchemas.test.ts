import { describe, expect, it } from "vitest";
import { SessionResponseSchema, SuccessResponseSchema } from "../src";

describe("response schemas", () => {
  it("should parse valid session responses", () => {
    expect(SessionResponseSchema.parse({ sessionId: "session-id" })).toEqual({
      sessionId: "session-id",
    });
  });

  it("should reject invalid session responses", () => {
    expect(SessionResponseSchema.safeParse({ sessionId: 1 }).success).toBe(false);
  });

  it("should parse valid success responses", () => {
    expect(SuccessResponseSchema.parse({ success: true })).toEqual({
      success: true,
    });
    expect(
      SuccessResponseSchema.parse({ success: false, reason: "invalid token" }),
    ).toEqual({ success: false, reason: "invalid token" });
  });

  it("should reject invalid success responses", () => {
    expect(SuccessResponseSchema.safeParse({ success: "yes" }).success).toBe(
      false,
    );
  });
});
