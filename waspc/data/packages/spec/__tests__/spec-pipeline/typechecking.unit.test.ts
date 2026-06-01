import { afterEach, describe, expect, test, vi } from "vitest";
import { typecheck } from "../../src/spec-pipeline/typecheck.js";
import { SpecUserError } from "../../src/spec/specUserError.js";

describe("typecheck", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  test("returns the callback value when no source files are added", async () => {
    const result = await runTypecheck(async () => 42);

    expect(result).toBe(42);
  });

  test("returns the callback value when added source files are type-clean", async () => {
    const result = await runTypecheck(async ({ addSourceFile }) => {
      addSourceFile("./clean.ts", `export const x: number = 1;\n`);
      return "ok";
    });

    expect(result).toBe("ok");
  });

  test("throws SpecUserError with formatted diagnostics when an added source file has a type error", async () => {
    const result = runTypecheck(async ({ addSourceFile }) => {
      addSourceFile("./bad.ts", `export const x: string = 123;\n`);
      return "should not reach";
    });

    await expect(result).rejects.toThrowError(SpecUserError);
    await expect(result).rejects.toThrowError("bad.ts");
    await expect(result).rejects.toThrowError("TS2322");
    await expect(result).rejects.toThrowError(
      "export const x: string = 123;",
    );
  });

  test("re-throws a ParseError from the callback without typechecking", async () => {
    const consoleError = vi.spyOn(console, "error");

    // Jiti parse errors are not any specific `Error` subclass, just regular
    // `Error` that start with "ParseError".
    const parseError = new Error("ParseError: unexpected token");

    await expect(
      runTypecheck(async ({ addSourceFile }) => {
        addSourceFile("./bad.ts", `export const x: string = 123;\n`);
        throw parseError;
      }),
    ).rejects.toBe(parseError);

    expect(consoleError).not.toHaveBeenCalled();
  });

  test("re-throws a non-parse runtime error when there are no type errors", async () => {
    const runtimeError = new Error("runtime boom");

    await expect(
      runTypecheck(async () => {
        throw runtimeError;
      }),
    ).rejects.toBe(runtimeError);
  });

  test("type errors take precedence over a runtime error from the callback", async () => {
    const result = runTypecheck(async ({ addSourceFile }) => {
      addSourceFile("./bad.ts", `export const x: string = 123;\n`);
      throw new Error("runtime boom");
    });

    await expect(result).rejects.toThrowError(SpecUserError);
    await expect(result).rejects.toThrowError("TS2322");
    await expect(result).rejects.not.toThrowError("runtime boom");
  });

  test("reports type errors from any added source file", async () => {
    const result = runTypecheck(async ({ addSourceFile }) => {
      addSourceFile("./main.ts", `export const x: number = 1;\n`);
      addSourceFile("./helper.ts", `export const broken: string = 123;\n`);
      return "unused";
    });

    await expect(result).rejects.toThrowError(SpecUserError);
    await expect(result).rejects.toThrowError("helper.ts");
    await expect(result).rejects.toThrowError("TS2322");
  });

  test("addSourceFile called twice with the same path uses the latest contents", async () => {
    const result = await runTypecheck(async ({ addSourceFile }) => {
      addSourceFile("./shared.ts", `export const x: string = 123;\n`);
      addSourceFile("./shared.ts", `export const x: number = 1;\n`);
      return "ok";
    });

    expect(result).toBe("ok");
  });

  test("does not print formatted diagnostics to stderr when type errors exist", async () => {
    const consoleError = vi.spyOn(console, "error");

    await expect(
      runTypecheck(async ({ addSourceFile }) => {
        addSourceFile("./bad.ts", `export const x: string = 123;\n`);
        return "unused";
      }),
    ).rejects.toThrow(SpecUserError);

    expect(consoleError).not.toHaveBeenCalled();
  });
});

function runTypecheck<T>(
  fn: (ctx: {
    addSourceFile: (path: string, code: string) => void;
  }) => Promise<T>,
): Promise<T> {
  return typecheck({ tsconfigPath: null }, fn);
}
