import { beforeEach, describe, expect, it, vi } from "vitest";
import { getAuthErrorMessage } from "../src/browser/auth/forms/authErrorMessage";
import { HttpError } from "../src/HttpError";

describe("getAuthErrorMessage", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  it("uses the message and the description of an `HttpError`", () => {
    const error = new HttpError(422, "Validation failed", {
      data: { message: "The password is too short." },
    });

    expect(getAuthErrorMessage(error)).toEqual({
      title: "Validation failed",
      description: "The password is too short.",
    });
    expect(console.error).not.toHaveBeenCalled();
  });

  it("reports an `HttpError` with a status of 500 or above", () => {
    const error = new HttpError(500, "Internal server error");

    expect(getAuthErrorMessage(error)).toEqual({
      title: "Internal server error",
      description: undefined,
    });
    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in authForm:",
      error,
    );
  });

  it("reports an error that isn't an `HttpError`", () => {
    const error = new TypeError("Failed to fetch");

    getAuthErrorMessage(error);

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in authForm:",
      error,
    );
  });
});
