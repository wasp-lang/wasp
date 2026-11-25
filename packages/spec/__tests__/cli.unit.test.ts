import { describe, expect, test } from "vitest";
import { type AnalyzeArgs, makeCli } from "../src/cli.js";

describe("makeCli", () => {
  test("should parse arguments correctly", async () => {
    await expectParseToSucceed([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "/project",
      "output.json",
      JSON.stringify(["entity1"]),
    ]);
  });

  test("should parse 0 entities correctly", async () => {
    await expectParseToSucceed([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "/project",
      "output.json",
      "[]",
    ]);
  });

  test("should throw an error if an argument is missing", async () => {
    await expectParseToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "/project",
      "output.json",
    ]);
  });

  test("should throw an error if there is an extra argument", async () => {
    await expectParseToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "/project",
      "output.json",
      "[]",
      "extraArg",
    ]);
  });

  test("should throw an error if command is unsupported", async () => {
    await expectParseToError([
      "compile",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "/project",
      "output.json",
      "[]",
    ]);
  });

  test("should throw an error if the entity names JSON is not an array", async () => {
    await expectParseToError([
      "analyze",
      "main.wasp.ts",
      "tsconfig.wasp.json",
      "/project",
      "output.json",
      JSON.stringify({ entity1: "entity1" }),
    ]);
  });

  async function expectParseToSucceed(args: string[]) {
    const [
      _command,
      waspTsSpecPath,
      tsconfigPath,
      projectRootDir,
      specResultPath,
      entityNames,
    ] = args;
    expect(await parse(args)).toEqual({
      waspTsSpecPath,
      tsconfigPath,
      projectRootDir,
      specResultPath,
      entityNames: entityNames && JSON.parse(entityNames),
    });
  }

  async function expectParseToError(args: string[]) {
    await expect(parse(args)).rejects.toThrowError();
  }

  async function parse(args: string[]): Promise<AnalyzeArgs | undefined> {
    let parsedArgs: AnalyzeArgs | undefined;
    const program = makeCli(async (analyzeArgs) => {
      parsedArgs = analyzeArgs;
    });
    // Throw instead of exiting the process, and keep the usage errors quiet.
    for (const command of [program, ...program.commands]) {
      command.exitOverride().configureOutput({ writeErr: () => {} });
    }
    await program.parseAsync(args, { from: "user" });
    return parsedArgs;
  }
});
