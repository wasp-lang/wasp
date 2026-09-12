import { afterEach, describe, expect, test } from "vitest";
import { readWaspInfo } from "../../src/common/waspProject.js";
import {
  makeProjectWithoutWaspInfo,
  makeProjectWithWaspInfo,
  removeTempProjects,
} from "./tempWaspProject.js";

afterEach(removeTempProjects);

describe("readWaspInfo", () => {
  test.each(["single", "split"] as const)(
    "reads the %s deployment mode",
    (deploymentMode) => {
      const projectDir = makeProjectWithWaspInfo(
        JSON.stringify({ deploymentMode }),
      );

      expect(readWaspInfo(projectDir).deploymentMode).toBe(deploymentMode);
    },
  );

  test("reads no deployment mode when the field is missing", () => {
    const projectDir = makeProjectWithWaspInfo(
      JSON.stringify({ waspVersion: "0.25.0" }),
    );

    expect(readWaspInfo(projectDir).deploymentMode).toBeUndefined();
  });

  test("rejects a missing .waspinfo", () => {
    const projectDir = makeProjectWithoutWaspInfo();

    expect(() => readWaspInfo(projectDir)).toThrow("Could not read");
  });

  test.each([
    ["malformed", "not-json"],
    ["non-object", "[]"],
    ["unknown deployment mode", JSON.stringify({ deploymentMode: "other" })],
  ])("rejects a %s .waspinfo", (_caseName, contents) => {
    const projectDir = makeProjectWithWaspInfo(contents);

    expect(() => readWaspInfo(projectDir)).toThrow(
      "is not a valid .waspinfo file",
    );
  });
});
