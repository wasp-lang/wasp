import { stripVTControlCharacters } from "node:util";
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

  test("throws SpecUserError when an added source file has a type error", async () => {
    vi.spyOn(console, "error");

    await expect(
      runTypecheck(async ({ addSourceFile }) => {
        addSourceFile("./bad.ts", `export const x: string = 123;\n`);
        return "should not reach";
      }),
    ).rejects.toThrow(new SpecUserError("Type errors found"));
  });

  test("re-throws a ParseError from the callback without typechecking", async () => {
    const consoleError = vi.spyOn(console, "error");
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
    vi.spyOn(console, "error");

    await expect(
      runTypecheck(async ({ addSourceFile }) => {
        addSourceFile("./bad.ts", `export const x: string = 123;\n`);
        throw new Error("runtime boom");
      }),
    ).rejects.toThrow(new SpecUserError("Type errors found"));
  });

  test("reports type errors from any added source file", async () => {
    vi.spyOn(console, "error");

    await expect(
      runTypecheck(async ({ addSourceFile }) => {
        addSourceFile("./main.ts", `export const x: number = 1;\n`);
        addSourceFile("./helper.ts", `export const broken: string = 123;\n`);
        return "unused";
      }),
    ).rejects.toThrow(new SpecUserError("Type errors found"));
  });

  test("addSourceFile called twice with the same path uses the latest contents", async () => {
    const result = await runTypecheck(async ({ addSourceFile }) => {
      addSourceFile("./shared.ts", `export const x: string = 123;\n`);
      addSourceFile("./shared.ts", `export const x: number = 1;\n`);
      return "ok";
    });

    expect(result).toBe("ok");
  });

  test("prints formatted diagnostics to stderr when type errors exist", async () => {
    const consoleError = vi.spyOn(console, "error");

    await expect(
      runTypecheck(async ({ addSourceFile }) => {
        addSourceFile("./bad.ts", `export const x: string = 123;\n`);
        return "unused";
      }),
    ).rejects.toThrow(SpecUserError);

    expect(consoleError).toHaveBeenCalledTimes(1);

    const rawOutput = consoleError.mock.calls[0]?.[0] as string;
    const output = stripVTControlCharacters(rawOutput);
    expect(output).toMatchInlineSnapshot(`
      "bad.ts:1:14 - error TS2322: Type 'number' is not assignable to type 'string'.

      1 export const x: string = 123;
                     ~
      "
    `);
  });
});

function runTypecheck<T>(
  fn: (ctx: {
    addSourceFile: (path: string, code: string) => void;
  }) => Promise<T>,
): Promise<T> {
  return typecheck({ tsconfigPath: null }, fn);
}
