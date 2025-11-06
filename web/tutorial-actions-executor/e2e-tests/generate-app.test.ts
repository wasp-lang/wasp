import path from "path";
import { describe, expect, it } from "vitest";
import { $, fs, glob } from "zx";

const TEST_APP_NAME = "e2e-test-app";
const TEST_OUTPUT_DIR = "./e2e-tests/.result";
const TEST_TUTORIAL_DIR = "./e2e-tests/fixtures/tutorial";
const WASP_CLI_COMMAND = process.env.WASP_CLI_COMMAND ?? "wasp-cli";

describe("generate-app e2e", () => {
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

      function getProjectFileContent(filePath: string): string {
        return fs.readFileSync(path.join(projectDirPath, filePath), "utf-8");
      }

      const fileList = await getProjectFileList(projectDirPath);
      expect(fileList).toMatchSnapshot("file-structure");

      const testUtilsContent = getProjectFileContent("src/testUtils.ts");
      expect(testUtilsContent).toMatchSnapshot("test-utils");

      const schemaContent = getProjectFileContent("schema.prisma");
      expect(schemaContent).toMatchSnapshot("prisma-schema");

      const gitLog = await getGitLog(projectDirPath);
      expect(gitLog).toMatchSnapshot("git-commits");
    },
    60 * 1000, // Test timeout
  );
});

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
  return content.replace(/\d{14}/g, "TIMESTAMP");
}
