import { describe, expect, test } from "vitest";
import {
  stripClientSuffix,
  stripServerSuffix,
} from "../../../src/providers/fly/tomlFile.js";

describe("stripServerSuffix", () => {
  test("strips -server suffix", () => {
    expect(stripServerSuffix("my-app-server")).toBe("my-app");
  });

  test("only strips the first occurrence (String.replace behavior)", () => {
    expect(stripServerSuffix("my-server-app-server")).toBe("my-app-server");
  });

  test("returns unchanged if no -server suffix", () => {
    expect(stripServerSuffix("my-app")).toBe("my-app");
  });
});

describe("stripClientSuffix", () => {
  test("strips -client suffix", () => {
    expect(stripClientSuffix("my-app-client")).toBe("my-app");
  });

  test("only strips the first occurrence (String.replace behavior)", () => {
    expect(stripClientSuffix("my-client-app-client")).toBe("my-app-client");
  });

  test("returns unchanged if no -client suffix", () => {
    expect(stripClientSuffix("my-app")).toBe("my-app");
  });
});
