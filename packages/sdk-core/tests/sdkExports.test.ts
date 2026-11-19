import { describe, expect, it } from "vitest";
import { resolveSdkImport } from "../src/node/sdk/sdkExports";

describe("resolveSdkImport", () => {
  const sdkDir = "/project/.wasp/out/sdk/wasp";
  const sdkExports = {
    "./client/operations": "./dist/client/operations/index.js",
    "./client/api": "./dist/api/index.js",
    "./server/crud/*": "./dist/server/crud/*.js",
  };
  const resolve = (importSpecifier: string) =>
    resolveSdkImport(sdkDir, sdkExports, importSpecifier);

  it("resolves an export to the file it points at", () => {
    expect(resolve("wasp/client/operations")).toBe(
      `${sdkDir}/dist/client/operations/index.js`,
    );
    expect(resolve("wasp/client/api")).toBe(`${sdkDir}/dist/api/index.js`);
  });

  it("fills in the wildcard of an export", () => {
    expect(resolve("wasp/server/crud/tasks")).toBe(
      `${sdkDir}/dist/server/crud/tasks.js`,
    );
    expect(resolve("wasp/server/crud/")).toBeUndefined();
  });

  it("doesn't resolve what the SDK doesn't export", () => {
    expect(resolve("wasp/client/operations/internal")).toBeUndefined();
    expect(resolve("wasp/dist/client/operations/index.js")).toBeUndefined();
    expect(resolve("wasp")).toBeUndefined();
  });

  it("doesn't resolve imports of other packages", () => {
    expect(resolve("react")).toBeUndefined();
    expect(resolve("wasp-config/client/operations")).toBeUndefined();
    expect(resolve("./client/operations")).toBeUndefined();
  });
});
