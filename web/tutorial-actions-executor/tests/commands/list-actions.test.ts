import { describe, expect, it } from "vitest";

import type { Action, ActionId, MdxFilePath } from "../../src/actions/actions";
import { groupActionsBySourceFile } from "../../src/commands/list-actions/index";

describe("groupActionsBySourceFile", () => {
  it("should group actions by source file basename", () => {
    const actions: Action[] = [
      {
        id: "action1" as ActionId,
        sourceTutorialFilePath: "/path/to/01-intro.md" as MdxFilePath,
        kind: "INIT_APP",
        waspStarterTemplateName: "basic",
      },
      {
        id: "action2" as ActionId,
        sourceTutorialFilePath: "/path/to/01-intro.md" as MdxFilePath,
        kind: "APPLY_PATCH",
        displayName: "Add auth",
        patchFilePath: "/patches/auth.patch" as any,
      },
      {
        id: "action3" as ActionId,
        sourceTutorialFilePath: "/path/to/02-setup.md" as MdxFilePath,
        kind: "MIGRATE_DB",
      },
    ];

    const result = groupActionsBySourceFile(actions);

    expect(result.size).toBe(2);
    expect(result.get("01-intro.md")).toEqual([actions[0], actions[1]]);
    expect(result.get("02-setup.md")).toEqual([actions[2]]);
  });

  it("should handle empty actions array", () => {
    const actions: Action[] = [];
    const result = groupActionsBySourceFile(actions);
    expect(result.size).toBe(0);
  });
});
