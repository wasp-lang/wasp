import { describe, expect, it } from "vitest";
import {
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  ensureValidUsername,
  ValidationError,
} from "../src/validation";

describe("validation", () => {
  it("should accept valid email", () => {
    expect(() => ensureValidEmail({ email: "test@example.com" })).not.toThrow();
  });

  it("should reject missing email", () => {
    expectValidationError(() => ensureValidEmail({}), "email must be present");
  });

  it("should reject invalid email", () => {
    expectValidationError(
      () => ensureValidEmail({ email: "not-an-email" }),
      "email must be a valid email",
    );
  });

  it("should reject email with extra text", () => {
    expectValidationError(
      () => ensureValidEmail({ email: "test@example.com nope" }),
      "email must be a valid email",
    );
  });

  it("should accept valid username", () => {
    expect(() => ensureValidUsername({ username: "miho" })).not.toThrow();
  });

  it("should reject missing username", () => {
    expectValidationError(
      () => ensureValidUsername({}),
      "username must be present",
    );
  });

  it("should accept present password", () => {
    expect(() => ensurePasswordIsPresent({ password: "secret" })).not.toThrow();
  });

  it("should reject missing password", () => {
    expectValidationError(
      () => ensurePasswordIsPresent({}),
      "password must be present",
    );
  });

  it("should accept a valid password", () => {
    expect(() => ensureValidPassword({ password: "password1" })).not.toThrow();
  });

  it("should reject too short passwords", () => {
    expectValidationError(
      () => ensureValidPassword({ password: "short1" }),
      "password must be at least 8 characters",
    );
  });

  it("should reject passwords without numbers", () => {
    expectValidationError(
      () => ensureValidPassword({ password: "password" }),
      "password must contain a number",
    );
  });

  it("should accept present token", () => {
    expect(() => ensureTokenIsPresent({ token: "token" })).not.toThrow();
  });

  it("should reject missing token", () => {
    expectValidationError(() => ensureTokenIsPresent({}), "token must be present");
  });
});

function expectValidationError(validate: () => void, message: string): void {
  expect(validate).toThrow(ValidationError);
  expect(validate).toThrow(message);
}
