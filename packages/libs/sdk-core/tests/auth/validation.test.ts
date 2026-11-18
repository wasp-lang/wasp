import { describe, expect, it } from "vitest";
import {
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  ensureValidUsername,
} from "../../src/auth/validation";

describe("ensureValidPassword", () => {
  it("accepts a password with at least 8 characters and a number", () => {
    expect(() => ensureValidPassword({ password: "password1" })).not.toThrow();
  });

  it.each([
    [
      "is too short",
      { password: "pass1" },
      "Password must be at least 8 characters",
    ],
    [
      "has no number",
      { password: "password" },
      "Password must contain a number",
    ],
  ])("rejects a password that %s", (_, args, expectedMessage) => {
    expect(() => ensureValidPassword(args)).toThrow(
      validationError(expectedMessage),
    );
  });
});

describe("ensureValidEmail", () => {
  it("accepts an internationalized email", () => {
    expect(() => ensureValidEmail({ email: "用户@例子.广告" })).not.toThrow();
  });

  it.each([
    ["is missing", {}, "Email must be present"],
    ["isn't an email", { email: "user" }, "Email must be a valid email"],
  ])("rejects an email that %s", (_, args, expectedMessage) => {
    expect(() => ensureValidEmail(args)).toThrow(
      validationError(expectedMessage),
    );
  });
});

describe.each([
  ["ensureValidUsername", ensureValidUsername, "Username"],
  ["ensurePasswordIsPresent", ensurePasswordIsPresent, "Password"],
  ["ensureTokenIsPresent", ensureTokenIsPresent, "Token"],
])("%s", (_, ensure, field) => {
  it("rejects a missing value", () => {
    expect(() => ensure({})).toThrow(
      validationError(`${field} must be present`),
    );
  });
});

function validationError(message: string) {
  return expect.objectContaining({
    statusCode: 422,
    message: "Validation failed",
    data: { message },
  });
}
