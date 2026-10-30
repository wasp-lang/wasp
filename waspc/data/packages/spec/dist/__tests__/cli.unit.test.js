import { describe, expect, test } from "vitest";
import { parseProcessArgsOrThrow } from "../src/cli.js";
describe("parseProcessArgsOrThrow", () => {
    test("should parse arguments correctly", () => {
        expectParseProcessArgsToSucceed([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            "/project",
            "output.json",
            JSON.stringify(["entity1"]),
        ]);
    });
    test("should parse 0 entities correctly", () => {
        expectParseProcessArgsToSucceed([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            "/project",
            "output.json",
            "[]",
        ]);
    });
    test("should throw an error if less than 8 arguments", () => {
        expectParseProcessArgsToError([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            "/project",
            "output.json",
        ]);
    });
    test("should throw an error if more than 8 arguments", () => {
        expectParseProcessArgsToError([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            "/project",
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
            "/project",
            "output.json",
            "[]",
        ]);
    });
    test("should throw an error if waspTsSpecPath is not a string", () => {
        expectParseProcessArgsToError([
            "analyze",
            undefined,
            "tsconfig.wasp.json",
            "/project",
            "output.json",
            JSON.stringify(["entity1"]),
        ]);
    });
    test("should throw an error if tsconfigPath is not a string", () => {
        expectParseProcessArgsToError([
            "analyze",
            "main.wasp.ts",
            undefined,
            "/project",
            "output.json",
            JSON.stringify(["entity1"]),
        ]);
    });
    test("should throw an error if projectRootDir is not a string", () => {
        expectParseProcessArgsToError([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            undefined,
            "output.json",
            JSON.stringify(["entity1"]),
        ]);
    });
    test("should throw an error if specResultPath is not a string", () => {
        expectParseProcessArgsToError([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            "/project",
            undefined,
            JSON.stringify(["entity1"]),
        ]);
    });
    test("should throw an error if any entityNames is not a string", () => {
        expectParseProcessArgsToError([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            "/project",
            "output.json",
            undefined,
        ]);
    });
    test("should throw an error if the entity names JSON is not an array", () => {
        expectParseProcessArgsToError([
            "analyze",
            "main.wasp.ts",
            "tsconfig.wasp.json",
            "/project",
            "output.json",
            JSON.stringify({ entity1: "entity1" }),
        ]);
    });
    function expectParseProcessArgsToSucceed(args) {
        const result = parseProcessArgsOrThrow(["node", "run.js", ...args]);
        const [_command, waspTsSpecPath, tsconfigPath, projectRootDir, specResultPath, entityNames,] = args;
        expect(result).toEqual({
            waspTsSpecPath,
            tsconfigPath,
            projectRootDir,
            specResultPath,
            entityNames: entityNames && JSON.parse(entityNames),
        });
    }
    function expectParseProcessArgsToError(args) {
        expect(() => parseProcessArgsOrThrow(["node", "run.js", ...args])).toThrowError();
    }
});
