import fs from "fs";
import path from "path";

import parseGitDiff from "parse-git-diff";

import type { ActionCommon, ApplyPatchAction } from "./index";

export function createApplyPatchAction(
  commonActionData: ActionCommon,
): ApplyPatchAction {
  const patchContentPath = path.resolve(
    "../docs/tutorial",
    "patches",
    `step-${commonActionData.step}.patch`,
  );

  const patch = fs.readFileSync(patchContentPath, "utf-8");

  const parsedPatch = parseGitDiff(patch);

  if (parsedPatch.files.length === 0 || parsedPatch.files[0] === undefined) {
    throw new Error("Invalid patch: no changes found");
  }

  if (parsedPatch.files.length > 1) {
    throw new Error("Invalid patch: multiple files changed");
  }

  if (parsedPatch.files[0].type !== "ChangedFile") {
    throw new Error("Invalid patch: only file changes are supported");
  }

  const targetFilePath = parsedPatch.files[0].path;

  return {
    ...commonActionData,
    kind: "diff",
    targetFilePath,
    patchContentPath,
  };
}
