import path from "path";
import { describe, expect, it } from "vitest";
import { $, fs, glob } from "zx";

describe("generate-app e2e", () => {
  it(
    "should generate app from tutorial actions and match snapshots",
    async () => {
      const TEST_TUTORIAL_DIR = "./e2e-tests/fixtures/tutorial";
      const TEST_OUTPUT_DIR = "./e2e-tests/.result";
      const TEST_APP_NAME = "TestApp";

      await $`npm run tacte -- generate-app --app-name ${TEST_APP_NAME} --output-dir ${TEST_OUTPUT_DIR} --tutorial-dir ${TEST_TUTORIAL_DIR}`.verbose();

      const projectDirPath = path.join(TEST_OUTPUT_DIR, TEST_APP_NAME);

      // Verify generated file structure
      const fileList = await getProjectFileList(projectDirPath);
      expect(fileList).toMatchSnapshot("file-structure");

      // File added by patch in 02-patch.md
      const testUtilsPath = path.join(projectDirPath, "src/testUtils.ts");
      const testUtilsContent = fs.readFileSync(testUtilsPath, "utf-8");
      expect(testUtilsContent).toMatchSnapshot("test-utils");

      // Verify git commit history
      const gitLog = await getGitLog(projectDirPath);
      expect(gitLog).toMatchSnapshot("git-commits");
    },
    3 * 60 * 1000, // Test timeout
  );
});

async function getProjectFileList(projectDirPath: string): Promise<string[]> {
  const files = await glob("**/*", {
    cwd: projectDirPath,
    ignore: ["node_modules/**", ".wasp/**", ".git/**"],
    onlyFiles: true,
  });
  return files.sort();
}

async function getGitLog(projectDirPath: string): Promise<string[]> {
  const gitLog =
    await $`git -C ${projectDirPath} log --format=%an%n%ae%n%s%n%b%n---`
      .quiet()
      .text();

  return gitLog
    .split("---")
    .map((commit) => commit.trim())
    .filter((commit) => !!commit);
}
