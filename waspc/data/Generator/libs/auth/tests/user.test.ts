import { describe, expect, it } from "vitest";
import {
  getEmail,
  getFirstProviderUserId,
  getUsername,
  makeAuthUserIfPossible,
} from "../src/user";

describe("user helpers", () => {
  it("should get email identity", () => {
    expect(getEmail(createUserWithIdentities())).toBe("test@example.com");
  });

  it("should get username identity", () => {
    expect(getUsername(createUserWithIdentities())).toBe("miho");
  });

  it("should get first provider user ID", () => {
    expect(getFirstProviderUserId(createUserWithIdentities())).toBe(
      "test@example.com",
    );
  });

  it("should return null for missing auth data", () => {
    expect(getEmail({ auth: null })).toBeNull();
    expect(getUsername({ auth: null })).toBeNull();
    expect(getFirstProviderUserId({ auth: null })).toBeNull();
  });

  it("should return null for empty identities", () => {
    const user = { auth: { identities: [] } };

    expect(getEmail(user)).toBeNull();
    expect(getUsername(user)).toBeNull();
    expect(getFirstProviderUserId(user)).toBeNull();
  });

  it("should create auth user helper", () => {
    const authUser = makeAuthUserIfPossible({
      name: "Miho",
      identities: {
        email: { id: "test@example.com" },
        github: null,
      },
    });

    expect(authUser?.name).toBe("Miho");
    expect(authUser?.getFirstProviderUserId()).toBe("test@example.com");
  });

  it("should return null when auth user data is missing", () => {
    expect(makeAuthUserIfPossible(null)).toBeNull();
  });
});

function createUserWithIdentities() {
  return {
    auth: {
      identities: [
        { providerName: "email", providerUserId: "test@example.com" },
        { providerName: "username", providerUserId: "miho" },
      ],
    },
  };
}
