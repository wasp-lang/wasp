import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import type { WaspProjectDir } from "../../src/common/brandedTypes.js";

const projectDirs: string[] = [];

export function makeProjectWithWaspInfo(contents: string): WaspProjectDir {
  const projectDir = makeProjectWithoutWaspInfo();
  fs.writeFileSync(
    path.join(projectDir, ".wasp", "out", ".waspinfo"),
    contents,
  );
  return projectDir;
}

export function makeProjectWithoutWaspInfo(): WaspProjectDir {
  const projectDir = fs.mkdtempSync(
    path.join(os.tmpdir(), "wasp-deploy-test-"),
  );
  fs.mkdirSync(path.join(projectDir, ".wasp", "out"), { recursive: true });
  projectDirs.push(projectDir);
  return projectDir as WaspProjectDir;
}

export function removeTempProjects(): void {
  for (const projectDir of projectDirs.splice(0)) {
    fs.rmSync(projectDir, { recursive: true, force: true });
  }
}
