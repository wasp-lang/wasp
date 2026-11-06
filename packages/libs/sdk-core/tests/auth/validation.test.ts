import { describe, expect, it } from "vitest";
import * as z from "zod";
import {
  defaultPasswordValidationSchema,
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  ensureValidUsername,
} from "../../src/auth/validation";

describe("ensureValidPassword with the default schema", () => {
  it("accepts a password with at least 8 characters and a number", () => {
    expect(() =>
      ensureValidPassword(
        { password: "password1" },
        defaultPasswordValidationSchema,
      ),
    ).not.toThrow();
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
    expect(() =>
      ensureValidPassword(args, defaultPasswordValidationSchema),
    ).toThrow(validationError(expectedMessage));
  });
});

describe("ensureValidPassword with a custom schema", () => {
  const schema = z.string().min(12, "Use at least 12 characters");

  it("rejects with the schema's message", () => {
    expect(() => ensureValidPassword({ password: "short" }, schema)).toThrow(
      validationError("Use at least 12 characters"),
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
    expect(() => ensureValidEmail(args)).toThrow(
      validationError(expectedMessage),
    );
  });
});

describe.each([
  ["ensureValidUsername", ensureValidUsername, "username"],
  ["ensurePasswordIsPresent", ensurePasswordIsPresent, "password"],
  ["ensureTokenIsPresent", ensureTokenIsPresent, "token"],
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
