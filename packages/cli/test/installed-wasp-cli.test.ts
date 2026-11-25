import * as path from "node:path";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import {
  handOverTo,
  type InstalledWaspCli,
  isSameWaspCli,
  notifyIfVersionsDiffer,
} from "../src/installed-wasp-cli.js";
import { makeWaspCliPackage, useTmpDir } from "./fixtures.js";

const getTmpDir = useTmpDir();

// A global Wasp CLI, installed somewhere else than the project.
const globalWaspCli: InstalledWaspCli = {
  runModulePath: "/global/node_modules/@wasp/cli/dist/run-wasp-cli.js",
  version: "0.27.0",
};

describe("isSameWaspCli", () => {
  test("is true for the same install, whatever the version says", () => {
    expect(
      isSameWaspCli({ ...globalWaspCli, version: "0.28.0" }, globalWaspCli),
    ).toBe(true);
  });

  test("is false for a CLI installed somewhere else, even at the same version", () => {
    const otherWaspCli: InstalledWaspCli = {
      runModulePath: "/app/node_modules/@wasp/cli/dist/run-wasp-cli.js",
      version: globalWaspCli.version,
    };

    expect(isSameWaspCli(otherWaspCli, globalWaspCli)).toBe(false);
  });
});

describe("handOverTo", () => {
  test("calls the other CLI's runWaspCli and returns its exit code", async () => {
    const runModulePath = await makeWaspCliPackage(
      path.join(getTmpDir(), "node_modules"),
      "0.27.0",
    );

    expect(await handOverTo({ runModulePath, version: "0.27.0" })).toBe(7);
  });
});

describe("notifyIfVersionsDiffer", () => {
  const projectWaspCli: InstalledWaspCli = {
    runModulePath: "/app/node_modules/@wasp/cli/dist/run-wasp-cli.js",
    version: "0.28.0",
  };
  let consoleError: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    consoleError = vi.spyOn(console, "error").mockImplementation(() => {});
  });

  afterEach(() => {
    consoleError.mockRestore();
  });

  test("says which version runs when it differs from the called CLI", () => {
    notifyIfVersionsDiffer(projectWaspCli, globalWaspCli, ["start"]);

    expect(consoleError).toHaveBeenCalledWith(
      "Running Wasp 0.28.0 from this project. The Wasp CLI you called is 0.27.0.",
    );
  });

  test("stays silent when the versions match", () => {
    notifyIfVersionsDiffer(
      { ...projectWaspCli, version: globalWaspCli.version },
      globalWaspCli,
      ["start"],
    );

    expect(consoleError).not.toHaveBeenCalled();
  });

  test("stays silent for a bash completion request, even when the versions differ", () => {
    notifyIfVersionsDiffer(projectWaspCli, globalWaspCli, ["completion:list"]);

    expect(consoleError).not.toHaveBeenCalled();
  });
});
