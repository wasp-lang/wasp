import { describe, expect, test } from "vitest";

import {
  assertVercelAppNameIsValid,
  getClientAppUrl,
  getClientProjectName,
  getServerAppUrl,
  getServerProjectName,
} from "../../../src/providers/vercel/brandedUrls.js";

describe("Vercel project name prediction", () => {
  test("derives the deterministic <app>-server project name", () => {
    expect(getServerProjectName("my-app")).toBe("my-app-server");
  });

  test("derives the deterministic <app>-client project name", () => {
    expect(getClientProjectName("my-app")).toBe("my-app-client");
  });
});

describe("Vercel production URL prediction", () => {
  // Deterministic URLs derived from the project names are what solve the
  // client<->server chicken-and-egg: both URLs are known before either
  // deploy runs.
  test("predicts https://<app>-server.vercel.app for the server", () => {
    expect(getServerAppUrl("my-app")).toBe("https://my-app-server.vercel.app");
  });

  test("predicts https://<app>-client.vercel.app for the client", () => {
    expect(getClientAppUrl("my-app")).toBe("https://my-app-client.vercel.app");
  });

  test("prediction matches the project name it is derived from", () => {
    expect(getServerAppUrl("wvtest-launch-e2e")).toBe(
      `https://${getServerProjectName("wvtest-launch-e2e")}.vercel.app`,
    );
    expect(getClientAppUrl("wvtest-launch-e2e")).toBe(
      `https://${getClientProjectName("wvtest-launch-e2e")}.vercel.app`,
    );
  });
});

describe("assertVercelAppNameIsValid", () => {
  test("accepts lowercase names with digits, hyphens and dots", () => {
    expect(() => assertVercelAppNameIsValid("my-app-2")).not.toThrow();
    expect(() => assertVercelAppNameIsValid("app.v2")).not.toThrow();
  });

  test("rejects names with uppercase, spaces or a leading separator", () => {
    expect(() => assertVercelAppNameIsValid("MyApp")).toThrow();
    expect(() => assertVercelAppNameIsValid("my app")).toThrow();
    expect(() => assertVercelAppNameIsValid("-my-app")).toThrow();
    expect(() => assertVercelAppNameIsValid("")).toThrow();
  });

  test("rejects names too long to fit the -server/-client suffix", () => {
    expect(() => assertVercelAppNameIsValid("a".repeat(91))).toThrow();
    expect(() => assertVercelAppNameIsValid("a".repeat(90))).not.toThrow();
  });
});
