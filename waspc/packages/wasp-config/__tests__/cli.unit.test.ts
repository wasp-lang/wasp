import { describe, expect, test } from "vitest";
import { parseProcessArgsOrThrow } from "../src/cli.js";

describe("parseProcessArgsOrThrow", () => {
  test("should parse arguments correctly", () => {
    expectParseProcessArgsToSucceed([
      "main.wasp.js",
      "output.json",
      JSON.stringify(["entity1"]),
    ]);
  });

  test("should parse arguments correctly even if node has absolute path", () => {
    expectParseProcessArgsToSucceed([
      "main.wasp.js",
      "output.json",
      JSON.stringify(["entity1"]),
    ], "/usr/bin/node");
  });

  test("should parse 0 entities correctly", () => {
    expectParseProcessArgsToSucceed(["main.wasp.js", "output.json", "[]"]);
  });

  test("should throw an error if less than 5 arguments", () => {
    expectParseProcessArgsToError(["main.wasp.js", "output.json"]);
  });

  test("should throw an error if more than 5 arguments", () => {
    expectParseProcessArgsToError([
      "main.wasp.js",
      "output.json",
      "[]",
      "extraArg",
    ]);
  });

  test("should throw an error if waspTsSpecPath is not a string", () => {
    expectParseProcessArgsToError([
      undefined,
      "output.json",
      JSON.stringify(["entity1"]),
    ] as string[]);
  });

  test("should throw an error if outputFilePath is not a string", () => {
    expectParseProcessArgsToError([
      "main.wasp.js",
      undefined,
      JSON.stringify(["entity1"]),
    ] as string[]);
  });

  test("should throw an error if any entityNames is not a string", () => {
    expectParseProcessArgsToError([
      "main.wasp.js",
      "output.json",
      undefined,
    ] as string[]);
  });

  test("should throw an error if the entity names JSON is not an array", () => {
    expectParseProcessArgsToError([
      "main.wasp.js",
      "output.json",
      JSON.stringify({ entity1: "entity1" }),
    ]);
  });

  function expectParseProcessArgsToSucceed(args: string[], nodeExec: string = "node") {
    const result = parseProcessArgsOrThrow([nodeExec, "run.js", ...args]);

    const [waspTsSpecPath, outputFilePath, entityNames] = args;
    expect(result).toEqual({
      waspTsSpecPath,
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
