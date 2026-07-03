import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  consumeOriginalRoute,
  redirectToFixed,
  redirectToOriginalRoute,
  saveOriginalRoute,
} from "../src/browser/redirect";

function createFakeSessionStorage(): Storage {
  const store = new Map<string, string>();
  return {
    get length() {
      return store.size;
    },
    clear: () => store.clear(),
    getItem: (key) => store.get(key) ?? null,
    key: (index) => [...store.keys()][index] ?? null,
    removeItem: (key) => void store.delete(key),
    setItem: (key, value) => void store.set(key, value),
  };
}

describe("redirectToOriginalRoute", () => {
  it("should return the original route from the context", () => {
    expect(
      redirectToOriginalRoute()({ originalRoute: "/tasks/42?tab=details" }),
    ).toBe("/tasks/42?tab=details");
  });

  it("should preserve query params and hash", () => {
    expect(
      redirectToOriginalRoute()({ originalRoute: "/a/b?c=d&e=f#section" }),
    ).toBe("/a/b?c=d&e=f#section");
  });

  it("should fall back to / when there is no original route", () => {
    expect(redirectToOriginalRoute()({ originalRoute: undefined })).toBe("/");
  });

  it("should fall back to the provided fallback route when there is no original route", () => {
    expect(
      redirectToOriginalRoute({ fallback: "/dashboard" })({
        originalRoute: undefined,
      }),
    ).toBe("/dashboard");
  });

  it("should prefer the original route over the fallback route", () => {
    expect(
      redirectToOriginalRoute({ fallback: "/dashboard" })({
        originalRoute: "/tasks",
      }),
    ).toBe("/tasks");
  });
});

describe("redirectToFixed", () => {
  it("should return the fixed route when there is no original route", () => {
    expect(redirectToFixed("/home")({ originalRoute: undefined })).toBe(
      "/home",
    );
  });

  it("should return the fixed route even when there is an original route", () => {
    expect(redirectToFixed("/home")({ originalRoute: "/tasks" })).toBe("/home");
  });
});

describe("original route storage", () => {
  beforeEach(() => {
    vi.stubGlobal("sessionStorage", createFakeSessionStorage());
  });

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("should return undefined when no route was saved", () => {
    expect(consumeOriginalRoute()).toBeUndefined();
  });

  it("should return the saved route", () => {
    saveOriginalRoute("/tasks/42?tab=details#comments");
    expect(consumeOriginalRoute()).toBe("/tasks/42?tab=details#comments");
  });

  it("should clear the saved route once consumed", () => {
    saveOriginalRoute("/tasks");
    consumeOriginalRoute();
    expect(consumeOriginalRoute()).toBeUndefined();
  });

  it("should overwrite a previously saved route", () => {
    saveOriginalRoute("/first");
    saveOriginalRoute("/second");
    expect(consumeOriginalRoute()).toBe("/second");
  });

  it.each([
    ["absolute URL", "https://evil.com/phishing"],
    ["protocol-relative URL", "//evil.com/phishing"],
    ["javascript URL", "javascript:alert(1)"],
    ["route without a leading slash", "tasks/42"],
    ["empty string", ""],
  ])("should not save an unsafe route (%s)", (_description, unsafeRoute) => {
    saveOriginalRoute(unsafeRoute);
    expect(consumeOriginalRoute()).toBeUndefined();
  });

  it("should not return an unsafe route that was tampered into storage", () => {
    // Anything (e.g. a browser extension) can write to sessionStorage, so
    // consuming must validate the route again.
    saveOriginalRoute("/tasks");
    sessionStorage.setItem(sessionStorage.key(0)!, "https://evil.com/phishing");
    expect(consumeOriginalRoute()).toBeUndefined();
  });

  describe("when sessionStorage is not available (e.g. SSR)", () => {
    beforeEach(() => {
      vi.stubGlobal("sessionStorage", undefined);
    });

    it("should silently do nothing", () => {
      expect(() => saveOriginalRoute("/tasks")).not.toThrow();
      expect(consumeOriginalRoute()).toBeUndefined();
    });
  });
});
