import { describe, expect, test } from "vitest";
import { transformWaspTsSpecSource } from "../../../src/spec-pipeline/transform/index.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("transformWaspTsSpecSource", () => {
  const sourcePath = "/project/main.wasp.ts";

  test("adds a source-aware ref import helper and lowers ref imports", () => {
    const input = [
      `import { app } from "@wasp.sh/spec";`,
      `import MainPage from "./src/MainPage" with { type: "ref" };`,
      ``,
      `export default app({ name: "demo", title: "Demo", wasp: { version: "^0.16.0" }, decls: [] });`,
      ``,
    ].join("\n");

    expect(transform(input)).toBe(
      [
        `import { app, makeRefImport } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        `const MainPage = refImport({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
        `export default app({ name: "demo", title: "Demo", wasp: { version: "^0.16.0" }, decls: [] });`,
        ``,
      ].join("\n"),
    );
  });

  test("throws when there is no spec package import", () => {
    expect(() =>
      transform(
        `import MainPage from "./src/MainPage" with { type: "ref" };\n`,
      ),
    ).toThrowError(SpecUserError);
  });

  test("reports unsupported ref imports using original source line numbers", () => {
    const input = [
      `import { app } from "@wasp.sh/spec";`,
      `import "./src/setup" with { type: "ref" };`,
      ``,
    ].join("\n");

    expect(() => transform(input)).toThrowError(
      `${sourcePath}(2,1): error: Unsupported ref import "./src/setup". Side-effect imports are not supported.`,
    );
  });

  function transform(sourceText: string): string {
    return transformWaspTsSpecSource({ sourceText, sourcePath });
  }
});
