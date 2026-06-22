import { describe, expect, it } from "vitest";
import {
  AuthContext,
  useAuthContext,
  type ErrorMessage,
} from "../src/browser";

describe("browser exports", () => {
  it("should export auth UI context helpers", () => {
    expect(AuthContext).toBeDefined();
    expect(useAuthContext).toEqual(expect.any(Function));
  });

  it("should expose the error message type", () => {
    const errorMessage: ErrorMessage = {
      title: "Unable to log in",
      description: "Invalid credentials",
    };

    expect(errorMessage).toEqual({
      title: "Unable to log in",
      description: "Invalid credentials",
    });
  });
});
