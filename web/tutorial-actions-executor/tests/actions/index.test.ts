import { describe, expect, it } from "vitest";

import type {
  ActionId,
  BaseAction,
  MdxFilePath,
} from "../../src/actions/actions";
import {
  createApplyPatchAction,
  createInitAppAction,
  createMigrateDbAction,
} from "../../src/actions/index";
import type { PatchesDirPath } from "../../src/tutorialApp";

describe("createInitAppAction", () => {
  it("should create InitAppAction with correct properties", () => {
    const baseAction: BaseAction = {
      id: "test-init" as ActionId,
      sourceTutorialFilePath: "/docs/01-intro.md" as MdxFilePath,
    };

    const result = createInitAppAction(baseAction, "basic");

    expect(result).toEqual({
      id: "test-init",
      sourceTutorialFilePath: "/docs/01-intro.md",
      kind: "INIT_APP",
      waspStarterTemplateName: "basic",
    });
  });
});

describe("createMigrateDbAction", () => {
  it("should create MigrateDbAction with correct properties", () => {
    const baseAction: BaseAction = {
      id: "db-migrate" as ActionId,
      sourceTutorialFilePath: "/docs/02-database.md" as MdxFilePath,
    };

    const result = createMigrateDbAction(baseAction);

    expect(result).toEqual({
      id: "db-migrate",
      sourceTutorialFilePath: "/docs/02-database.md",
      kind: "MIGRATE_DB",
    });
  });
});

describe("createApplyPatchAction", () => {
  it("should create ApplyPatchAction with correct properties", () => {
    const baseAction: BaseAction = {
      id: "add-auth" as ActionId,
      sourceTutorialFilePath: "/docs/03-auth.md" as MdxFilePath,
    };
    const patchesDirPath = "/patches" as PatchesDirPath;

    const result = createApplyPatchAction(baseAction, patchesDirPath);

    expect(result).toEqual({
      id: "add-auth",
      sourceTutorialFilePath: "/docs/03-auth.md",
      kind: "APPLY_PATCH",
      displayName: "03-auth.md / add-auth",
      patchFilePath: "/patches/03-auth__add-auth.patch",
    });
  });
});
