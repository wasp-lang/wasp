import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, test } from "vitest";
import type { WaspProjectDir } from "../../src/common/brandedTypes.js";
import { getAppDeliveryMode } from "../../src/common/waspProject.js";

const projectDirs: string[] = [];

afterEach(() => {
  for (const projectDir of projectDirs.splice(0)) {
    fs.rmSync(projectDir, { recursive: true, force: true });
  }
});

describe("getAppDeliveryMode", () => {
  test.each(["integrated", "split"] as const)(
    "reads %s delivery mode",
    (deliveryMode) => {
      const projectDir = makeProject({ deliveryMode });

      expect(getAppDeliveryMode(projectDir)).toBe(deliveryMode);
    },
  );

  test.each([
    ["missing metadata", undefined],
    ["malformed metadata", "not-json"],
    ["missing delivery mode", JSON.stringify({})],
    ["unknown delivery mode", JSON.stringify({ deliveryMode: "other" })],
  ])("rejects %s", (_caseName, contents) => {
    const projectDir = makeProject(contents);

    expect(() => getAppDeliveryMode(projectDir)).toThrow("Run `wasp build`");
  });
});

function makeProject(waspInfo?: object | string): WaspProjectDir {
  const projectDir = fs.mkdtempSync(
    path.join(os.tmpdir(), "wasp-deploy-test-"),
  );
  const buildDir = path.join(projectDir, ".wasp", "out");
  fs.mkdirSync(buildDir, { recursive: true });
  projectDirs.push(projectDir);

  if (waspInfo !== undefined) {
    fs.writeFileSync(
      path.join(buildDir, ".waspinfo"),
      typeof waspInfo === "string" ? waspInfo : JSON.stringify(waspInfo),
    );
  }

  return projectDir as WaspProjectDir;
}
