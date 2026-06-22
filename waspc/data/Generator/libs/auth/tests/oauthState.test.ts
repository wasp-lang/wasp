import { describe, expect, it } from "vitest";
import {
  generateAndStoreOAuthState,
  getOAuthCookieName,
  getOAuthCookieOptions,
  getOAuthCookieValueFromHeader,
  validateAndGetOAuthState,
  type OAuthStateFieldName,
} from "../src/node/oauthState";

describe("OAuth state", () => {
  it("should generate and store OAuth2 state", () => {
    const store = createOAuthStateStore();

    const state = generateAndStoreOAuthState({
      oAuthType: "OAuth2",
      store,
    });

    expect(state.state.length).toBeGreaterThan(0);
    expect(store.get("state")).toBe(state.state);
  });

  it("should generate and store OAuth2 with PKCE state", () => {
    const store = createOAuthStateStore();

    const state = generateAndStoreOAuthState({
      oAuthType: "OAuth2WithPKCE",
      store,
    });

    expect(state.state.length).toBeGreaterThan(0);
    expect(state.codeVerifier.length).toBeGreaterThan(0);
    expect(store.get("state")).toBe(state.state);
    expect(store.get("codeVerifier")).toBe(state.codeVerifier);
  });

  it("should validate OAuth2 state", () => {
    const store = createOAuthStateStore();
    const generatedState = generateAndStoreOAuthState({
      oAuthType: "OAuth2",
      store,
    });

    expect(
      validateAndGetOAuthState({
        oAuthType: "OAuth2",
        code: "code",
        state: generatedState.state,
        store,
      }),
    ).toEqual({ code: "code", state: generatedState.state });
  });

  it("should validate OAuth2 with PKCE state", () => {
    const store = createOAuthStateStore();
    const generatedState = generateAndStoreOAuthState({
      oAuthType: "OAuth2WithPKCE",
      store,
    });

    expect(
      validateAndGetOAuthState({
        oAuthType: "OAuth2WithPKCE",
        code: "code",
        state: generatedState.state,
        store,
      }),
    ).toEqual({
      code: "code",
      state: generatedState.state,
      codeVerifier: generatedState.codeVerifier,
    });
  });

  it("should reject invalid code", () => {
    const store = createOAuthStateStore();
    const generatedState = generateAndStoreOAuthState({
      oAuthType: "OAuth2",
      store,
    });

    expect(() =>
      validateAndGetOAuthState({
        oAuthType: "OAuth2",
        code: undefined,
        state: generatedState.state,
        store,
      }),
    ).toThrow("Invalid code");
  });

  it("should reject invalid state", () => {
    const store = createOAuthStateStore();
    generateAndStoreOAuthState({
      oAuthType: "OAuth2",
      store,
    });

    expect(() =>
      validateAndGetOAuthState({
        oAuthType: "OAuth2",
        code: "code",
        state: "wrong-state",
        store,
      }),
    ).toThrow("Invalid state");
  });

  it("should reject missing PKCE verifier", () => {
    const store = createOAuthStateStore();
    const generatedState = generateAndStoreOAuthState({
      oAuthType: "OAuth2WithPKCE",
      store,
    });
    store.delete("codeVerifier");

    expect(() =>
      validateAndGetOAuthState({
        oAuthType: "OAuth2WithPKCE",
        code: "code",
        state: generatedState.state,
        store,
      }),
    ).toThrow("Missing code verifier");
  });
});

describe("OAuth cookies", () => {
  it("should build OAuth cookie names", () => {
    expect(getOAuthCookieName("google", "state")).toBe("google_state");
    expect(getOAuthCookieName("google", "codeVerifier")).toBe(
      "google_codeVerifier",
    );
  });

  it("should build development cookie options", () => {
    expect(getOAuthCookieOptions({ isDevelopment: true })).toEqual({
      httpOnly: true,
      secure: false,
      path: "/",
      maxAge: 60 * 60 * 1000,
    });
  });

  it("should build production cookie options", () => {
    expect(getOAuthCookieOptions({ isDevelopment: false })).toEqual({
      httpOnly: true,
      secure: true,
      path: "/",
      maxAge: 60 * 60 * 1000,
    });
  });

  it("should read OAuth cookie values from cookie headers", () => {
    expect(
      getOAuthCookieValueFromHeader(
        "google",
        "state",
        "google_state=state; google_codeVerifier=verifier",
      ),
    ).toBe("state");
  });
});

function createOAuthStateStore() {
  const values = new Map<OAuthStateFieldName, string>();
  return {
    set: (fieldName: OAuthStateFieldName, value: string) => {
      values.set(fieldName, value);
    },
    get: (fieldName: OAuthStateFieldName) => values.get(fieldName),
    delete: (fieldName: OAuthStateFieldName) => {
      values.delete(fieldName);
    },
  };
}
