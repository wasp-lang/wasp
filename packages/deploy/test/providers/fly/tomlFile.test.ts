import { describe, expect, test } from "vitest";
import {
  inferClientBasename,
  inferServerBasename,
} from "../../../src/providers/fly/tomlFile.js";

describe.each([
  { suffix: "-server", inferBasename: inferServerBasename },
  { suffix: "-client", inferBasename: inferClientBasename },
])("inferBasename for $suffix", ({ suffix, inferBasename }) => {
  test("strips the suffix", () => {
    const baseName = "my-app";
    expect(inferBasename(`${baseName}${suffix}`)).toBe(baseName);
  });

  test("only strips the trailing suffix when base name also contains it", () => {
    expect(inferBasename(`my${suffix}-app${suffix}`)).toBe(`my${suffix}-app`);
  });

  test("returns unchanged if no suffix present", () => {
    const baseName = "my-app";
    expect(inferBasename(baseName)).toBe(baseName);
  });
});
