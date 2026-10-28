import { describe, expect, it } from "vitest";
import { waspSpecAliasPlugin } from "../../src/spec-pipeline/waspSpecAliasPlugin.js";

describe("waspSpecAliasPlugin", () => {
  const plugin = waspSpecAliasPlugin();
  const resolveId = plugin.resolveId as (source: string) => unknown;

  it("resolves the package entry points to this package's own files, as externals", () => {
    expect(resolveId("@wasp/spec")).toEqual({
      id: expect.stringMatching(/\/src\/index\.(js|ts)$/),
      external: true,
    });
    expect(resolveId("@wasp/spec/internal")).toEqual({
      id: expect.stringMatching(/\/src\/internal\.(js|ts)$/),
      external: true,
    });
  });

  it("leaves other imports to the default resolver", () => {
    expect(resolveId("./src/MainPage")).toBeNull();
    expect(resolveId("react")).toBeNull();
    expect(resolveId("@wasp/spec/other")).toBeNull();
  });
});
