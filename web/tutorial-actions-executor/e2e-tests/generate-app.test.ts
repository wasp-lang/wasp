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

async function getGitLog(projectDirPath: string): Promise<string> {
  const gitLogResult =
    await $`git -C ${projectDirPath} log --format="%s%b" --name-only`.quiet();

  return gitLogResult.text();
}
