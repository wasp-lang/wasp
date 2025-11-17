import * as fs from "node:fs/promises";
import * as path from "node:path";
import { beforeAll, describe, expect, it } from "vitest";
import { $, glob } from "zx";

const currentDirPath = import.meta.dirname;

const TEST_APP_NAME = "e2e-test-app";
const TEST_OUTPUT_DIR = path.join(currentDirPath, ".result");
const TEST_TUTORIAL_DIR = path.join(currentDirPath, "fixtures", "tutorial");
const WASP_CLI_COMMAND = process.env.WASP_CLI_COMMAND ?? "wasp-cli";

describe("generate-app e2e", () => {
  beforeAll(async () => {
    await fs.rm(TEST_OUTPUT_DIR, { recursive: true, force: true });
  });

  it(
    "should generate app from tutorial actions and match snapshots",
    async () => {
      const generateAppOptions = [
        "--app-name",
        TEST_APP_NAME,
        "--output-dir",
        TEST_OUTPUT_DIR,
        "--tutorial-dir",
        TEST_TUTORIAL_DIR,
        "--wasp-cli-command",
        WASP_CLI_COMMAND,
      ];
      await $`npm run tacte -- generate-app ${generateAppOptions}`.verbose();

      const projectDirPath = path.join(TEST_OUTPUT_DIR, TEST_APP_NAME);

      expect(await getProjectFileList(projectDirPath)).toMatchSnapshot(
        "file-structure",
      );
      expect(
        await getProjectFileContent(projectDirPath, "src/testUtils.ts"),
      ).toMatchSnapshot("test-utils");
      expect(
        await getProjectFileContent(projectDirPath, "schema.prisma"),
      ).toMatchSnapshot("prisma-schema");
      expect(await getGitLog(projectDirPath)).toMatchSnapshot("git-commits");
    },
    60 * 1000, // Test timeout
  );
});

function getProjectFileContent(
  projectDirPath: string,
  pathInProject: string,
): Promise<string> {
  return fs.readFile(path.join(projectDirPath, pathInProject), "utf-8");
}

async function getProjectFileList(projectDirPath: string): Promise<string[]> {
  const files = await glob("**/*", {
    cwd: projectDirPath,
    ignore: ["node_modules/**", ".wasp/**", ".git/**"],
    onlyFiles: true,
  });
  return files.map(normalizeTimestamps).sort();
}

async function getGitLog(projectDirPath: string): Promise<string> {
  const gitLogResult =
    await $`git -C ${projectDirPath} log --format="%s%b" --name-only`.quiet();

  return normalizeTimestamps(gitLogResult.text());
}

/**
 * Normalizes timestamps in the given content by replacing them with a placeholder.
 * Prisma migration files contain timestamps which change between test runs.
 */
function normalizeTimestamps(content: string): string {
  return content.replace(/\d{14}/g, "NORMALIZED_TIMESTAMP_E2E");
}
