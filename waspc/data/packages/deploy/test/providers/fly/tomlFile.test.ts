import { describe, expect, test } from "vitest";
import {
  inferClientBasename,
  inferServerBasename,
} from "../../../src/providers/fly/tomlFile.js";

describe("inferServerBasename", () => {
  test("strips -server suffix", () => {
    const baseName = "my-app";
    expect(inferServerBasename(`${baseName}-server`)).toBe(baseName);
  });

  test("only strips the first occurrence (String.replace behavior)", () => {
    expect(inferServerBasename("my-server-app-server")).toBe("my-app-server");
  });

  test("returns unchanged if no -server suffix", () => {
    const baseName = "my-app";
    expect(inferServerBasename(baseName)).toBe(baseName);
  });
});

describe("inferClientBasename", () => {
  test("strips -client suffix", () => {
    const baseName = "my-app";
    expect(inferClientBasename(`${baseName}-client`)).toBe(baseName);
  });

  test("only strips the first occurrence (String.replace behavior)", () => {
    expect(inferClientBasename("my-client-app-client")).toBe("my-app-client");
  });

  test("returns unchanged if no -client suffix", () => {
    const baseName = "my-app";
    expect(inferClientBasename(baseName)).toBe(baseName);
  });
});
