import { writeFileSync } from "fs";
import { join } from "path";
import { afterEach, describe, expect, test, vi } from "vitest";
import { typecheck } from "../../src/spec-pipeline/typecheck.js";
import { SpecUserError } from "../../src/spec/specUserError.js";
import { makeTempProject, writeTsConfig } from "./testHelpers.js";

describe("typecheck", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  test("returns the callback value when no source files are added", async () => {
    const { tsconfigPath } = setupProject("typecheck-empty-");

    const result = await typecheck({ tsconfigPath }, async () => 42);

    expect(result).toBe(42);
  });

  test("returns the callback value when added source files are type-clean", async () => {
    const { tempDir, tsconfigPath } = setupProject("typecheck-clean-");

    const result = await typecheck(
      { tsconfigPath },
      async ({ addSourceFile }) => {
        addSourceFile(
          join(tempDir, "clean.ts"),
          `export const x: number = 1;\n`,
        );
        return "ok";
      },
    );

    expect(result).toBe("ok");
  });

  test("throws SpecUserError when an added source file has a type error", async () => {
    const { tempDir, tsconfigPath } = setupProject("typecheck-bad-type-");
    silenceConsoleError();

    await expect(
      typecheck({ tsconfigPath }, async ({ addSourceFile }) => {
        addSourceFile(
          join(tempDir, "bad.ts"),
          `export const x: string = 123;\n`,
        );
        return "should not reach";
      }),
    ).rejects.toThrowError(new SpecUserError("Type errors found"));
  });

  test("re-throws a ParseError from the callback without typechecking", async () => {
    const { tempDir, tsconfigPath } = setupProject("typecheck-parse-error-");
    const consoleError = silenceConsoleError();
    const parseError = new Error("ParseError: unexpected token");

    await expect(
      typecheck({ tsconfigPath }, async ({ addSourceFile }) => {
        addSourceFile(
          join(tempDir, "bad.ts"),
          `export const x: string = 123;\n`,
        );
        throw parseError;
      }),
    ).rejects.toBe(parseError);

    expect(consoleError).not.toHaveBeenCalled();
  });

  test("re-throws a non-parse runtime error when there are no type errors", async () => {
    const { tsconfigPath } = setupProject("typecheck-runtime-error-");
    const runtimeError = new Error("runtime boom");

    await expect(
      typecheck({ tsconfigPath }, async () => {
        throw runtimeError;
      }),
    ).rejects.toBe(runtimeError);
  });

  test("type errors take precedence over a runtime error from the callback", async () => {
    const { tempDir, tsconfigPath } = setupProject(
      "typecheck-type-vs-runtime-",
    );
    silenceConsoleError();

    await expect(
      typecheck({ tsconfigPath }, async ({ addSourceFile }) => {
        addSourceFile(
          join(tempDir, "bad.ts"),
          `export const x: string = 123;\n`,
        );
        throw new Error("runtime boom");
      }),
    ).rejects.toThrowError(new SpecUserError("Type errors found"));
  });

  test("addSourceFile called twice with the same path uses the latest contents", async () => {
    const { tempDir, tsconfigPath } = setupProject("typecheck-overwrite-");

    const result = await typecheck(
      { tsconfigPath },
      async ({ addSourceFile }) => {
        const path = join(tempDir, "shared.ts");
        addSourceFile(path, `export const x: string = 123;\n`);
        addSourceFile(path, `export const x: number = 1;\n`);
        return "ok";
      },
    );

    expect(result).toBe("ok");
  });

  test("prints formatted diagnostics to stderr when type errors exist", async () => {
    const { tempDir, tsconfigPath } = setupProject(
      "typecheck-diagnostics-output-",
    );
    const consoleError = silenceConsoleError();

    await expect(
      typecheck({ tsconfigPath }, async ({ addSourceFile }) => {
        addSourceFile(
          join(tempDir, "bad.ts"),
          `export const x: string = 123;\n`,
        );
        return "unused";
      }),
    ).rejects.toThrowError(SpecUserError);

    expect(consoleError).toHaveBeenCalledTimes(1);
    const [output] = consoleError.mock.calls[0]!;
    expect(typeof output).toBe("string");
    expect((output as string).length).toBeGreaterThan(0);
  });
});

function setupProject(prefix: string): {
  tempDir: string;
  tsconfigPath: string;
} {
  const tempDir = makeTempProject(prefix);
  // Satisfy the tsconfig include so the TS compiler does not emit TS18003
  // ("no inputs were found in config file"), which surfaces as a type error.
  writeFileSync(join(tempDir, "_placeholder.ts"), `export {};\n`);
  const tsconfigPath = join(tempDir, "tsconfig.json");
  writeTsConfig(tsconfigPath, "**/*.ts");
  return { tempDir, tsconfigPath };
}

function silenceConsoleError(): ReturnType<typeof vi.spyOn> {
  return vi.spyOn(console, "error").mockImplementation(() => {});
}
