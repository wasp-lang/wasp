import { describe, expect, test } from "vitest";
import { parseProcessArgsOrThrow } from "../src/cli.js";

describe("parseProcessArgsOrThrow", () => {
  test("should parse arguments correctly", () => {
    expectParseProcessArgsToSucceed([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "output.json",
      JSON.stringify(["entity1"]),
    ]);
  });

  test("should parse 0 entities correctly", () => {
    expectParseProcessArgsToSucceed([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "output.json",
      "[]",
    ]);
  });

  test("should throw an error if less than 7 arguments", () => {
    expectParseProcessArgsToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "output.json",
    ]);
  });

  test("should throw an error if more than 7 arguments", () => {
    expectParseProcessArgsToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "output.json",
      "[]",
      "extraArg",
    ]);
  });

  test("should throw an error if command is unsupported", () => {
    expectParseProcessArgsToError([
      "compile",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "output.json",
      "[]",
    ]);
  });

  test("should throw an error if waspTsSpecPath is not a string", () => {
    expectParseProcessArgsToError([
      "analyze",
      undefined,
      "tsconfig.wasp.json",
      "output.json",
      JSON.stringify(["entity1"]),
    ] as string[]);
  });

  test("should throw an error if tsconfigPath is not a string", () => {
    expectParseProcessArgsToError([
      "analyze",
      "main.wasp.ts",
      undefined,
      "output.json",
      JSON.stringify(["entity1"]),
    ] as string[]);
  });

  test("should throw an error if outputFilePath is not a string", () => {
    expectParseProcessArgsToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      undefined,
      JSON.stringify(["entity1"]),
    ] as string[]);
  });

  test("should throw an error if any entityNames is not a string", () => {
    expectParseProcessArgsToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "output.json",
      undefined,
    ] as string[]);
  });

  test("should throw an error if the entity names JSON is not an array", () => {
    expectParseProcessArgsToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "output.json",
      JSON.stringify({ entity1: "entity1" }),
    ]);
  });

  function expectParseProcessArgsToSucceed(args: string[]) {
    const result = parseProcessArgsOrThrow(["node", "run.js", ...args]);

    const [
      _command,
      waspTsSpecPath,
      tsconfigPath,
      outputFilePath,
      entityNames,
    ] = args;
    expect(result).toEqual({
      waspTsSpecPath,
      tsconfigPath,
      outputFilePath,
      entityNames: entityNames && JSON.parse(entityNames),
    });
  }

  function expectParseProcessArgsToError(args: string[]) {
    expect(() =>
      parseProcessArgsOrThrow(["node", "run.js", ...args]),
    ).toThrowError();
  }
});
