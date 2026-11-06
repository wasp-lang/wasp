import { describe, expect, it } from "vitest";
import {
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  ensureValidUsername,
} from "../../src/auth/validation";
import { HttpError } from "../../src/HttpError";

describe("ensureValidPassword", () => {
  it("accepts a password with at least 8 characters and a number", () => {
    expect(() => ensureValidPassword({ password: "password1" })).not.toThrow();
  });

  it.each([
    [
      "is too short",
      { password: "pass1" },
      "password must be at least 8 characters",
    ],
    [
      "has no number",
      { password: "password" },
      "password must contain a number",
    ],
  ])("rejects a password that %s", (_, args, expectedMessage) => {
    expect(getThrownValidationMessage(() => ensureValidPassword(args))).toBe(
      expectedMessage,
    );
  });
});

describe("ensureValidEmail", () => {
  it("accepts an internationalized email", () => {
    expect(() => ensureValidEmail({ email: "用户@例子.广告" })).not.toThrow();
  });

  it.each([
    ["is missing", {}, "email must be present"],
    ["isn't an email", { email: "user" }, "email must be a valid email"],
  ])("rejects an email that %s", (_, args, expectedMessage) => {
    expect(getThrownValidationMessage(() => ensureValidEmail(args))).toBe(
      expectedMessage,
    );
  });
});

describe.each([
  ["ensureValidUsername", ensureValidUsername, "username"],
  ["ensurePasswordIsPresent", ensurePasswordIsPresent, "password"],
  ["ensureTokenIsPresent", ensureTokenIsPresent, "token"],
])("%s", (_, ensure, field) => {
  it("rejects a missing value", () => {
    expect(getThrownValidationMessage(() => ensure({}))).toBe(
      `${field} must be present`,
    );
  });
});

function getThrownValidationMessage(fn: () => void): unknown {
  try {
    fn();
  } catch (error) {
    expect(error).toBeInstanceOf(HttpError);
    expect(error).toMatchObject({
      statusCode: 422,
      message: "Validation failed",
    });
    return (error as HttpError & { data: { message: unknown } }).data.message;
  }
  throw new Error("Expected a validation error");
}
