import { describe, expect, it } from "vitest";
import {
  AuthServiceError,
  createJWTHelpers,
  hashPassword,
  loginWithEmail,
  loginWithUsername,
  parseCookies,
  requestPasswordReset,
  resetPassword,
  signupWithEmail,
  signupWithUsername,
  TimeSpan,
  verifyEmail,
  verifyPassword,
} from "../src/node";

describe("node exports", () => {
  it("should export node auth helpers", () => {
    expect(createJWTHelpers).toEqual(expect.any(Function));
    expect(hashPassword).toEqual(expect.any(Function));
    expect(verifyPassword).toEqual(expect.any(Function));
    expect(TimeSpan).toEqual(expect.any(Function));
    expect(AuthServiceError).toEqual(expect.any(Function));
    expect(signupWithEmail).toEqual(expect.any(Function));
    expect(loginWithEmail).toEqual(expect.any(Function));
    expect(requestPasswordReset).toEqual(expect.any(Function));
    expect(resetPassword).toEqual(expect.any(Function));
    expect(verifyEmail).toEqual(expect.any(Function));
    expect(signupWithUsername).toEqual(expect.any(Function));
    expect(loginWithUsername).toEqual(expect.any(Function));
  });

  it("should export cookie parsing", () => {
    const cookies = parseCookies("sessionId=abc123; theme=dark");

    expect(cookies.get("sessionId")).toBe("abc123");
    expect(cookies.get("theme")).toBe("dark");
  });
});
