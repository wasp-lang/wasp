import fs from "fs";

import parseGitDiff from "parse-git-diff";

export function assertValidPatchFile(patchContentPath: string): void {
  const patch = fs.readFileSync(patchContentPath, "utf-8");

  const parsedPatch = parseGitDiff(patch);

  if (parsedPatch.files.length === 0 || parsedPatch.files[0] === undefined) {
    throw new Error("Invalid patch: no changes found");
  }
}
