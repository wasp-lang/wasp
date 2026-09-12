import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import type {
  WaspCliExe,
  WaspProjectDir,
} from "../../src/common/brandedTypes.js";
import {
  makeProjectWithWaspInfo,
  removeTempProjects,
} from "./tempWaspProject.js";

vi.mock("../../src/common/terminal.js", () => ({
  waspSays: vi.fn(),
}));

// `ensureWaspProjectIsBuilt` runs `wasp build` through zx, which the tests
// replace with a no-op.
vi.mock("../../src/common/zx.js", () => ({
  createCommandWithCwd: () => vi.fn().mockResolvedValue(undefined),
}));

import { waspSays } from "../../src/common/terminal.js";

const waspExe = "wasp" as WaspCliExe;

// The build result is cached per module instance, so every test gets a fresh one.
beforeEach(() => {
  vi.resetModules();
});

afterEach(() => {
  vi.clearAllMocks();
  removeTempProjects();
});

describe("ensureWaspProjectIsBuilt", () => {
  test.each(["single", "split"] as const)(
    "returns the %s deployment mode of the build",
    async (deploymentMode) => {
      const waspProjectDir = makeProjectWithWaspInfo(
        JSON.stringify({ deploymentMode }),
      );
      const ensureWaspProjectIsBuilt = await importEnsureWaspProjectIsBuilt();

      const result = await ensureWaspProjectIsBuilt({
        waspProjectDir,
        waspExe,
      });

      expect(result).toEqual({ deploymentMode });
      expect(waspSays).not.toHaveBeenCalledWith(
        expect.stringContaining("Assuming split mode"),
      );
    },
  );

  test("assumes split mode with a warning when the build has no deployment mode", async () => {
    const waspProjectDir = makeProjectWithWaspInfo(
      JSON.stringify({ waspVersion: "0.25.0" }),
    );
    const ensureWaspProjectIsBuilt = await importEnsureWaspProjectIsBuilt();

    const result = await ensureWaspProjectIsBuilt({ waspProjectDir, waspExe });

    expect(result).toEqual({ deploymentMode: "split" });
    expect(waspSays).toHaveBeenCalledWith(
      expect.stringContaining("Assuming split mode"),
    );
  });

  test("builds only once", async () => {
    const waspProjectDir = makeProjectWithWaspInfo(
      JSON.stringify({ deploymentMode: "single" }),
    );
    const ensureWaspProjectIsBuilt = await importEnsureWaspProjectIsBuilt();

    await ensureWaspProjectIsBuilt({ waspProjectDir, waspExe });
    await ensureWaspProjectIsBuilt({ waspProjectDir, waspExe });

    expect(waspSays).toHaveBeenCalledTimes(1);
  });
});

async function importEnsureWaspProjectIsBuilt(): Promise<
  (options: {
    waspProjectDir: WaspProjectDir;
    waspExe: WaspCliExe;
  }) => Promise<{ deploymentMode: "single" | "split" }>
> {
  const waspBuild = await import("../../src/common/waspBuild.js");
  return waspBuild.ensureWaspProjectIsBuilt;
}
