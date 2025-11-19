import { describe, expect, it } from "vitest";

import { assertValidPatch } from "../../src/actions/git";
import {
  additionPatch,
  binaryPatch,
  deletionPatch,
  multiFilePatch,
  noFilesPatch,
  singleFilePatch,
} from "./git.fixtures";

describe("assertValidPatch", () => {
  expectValidPatchNotToThrow(singleFilePatch);
  expectValidPatchNotToThrow(multiFilePatch);
  expectValidPatchNotToThrow(deletionPatch);
  expectValidPatchNotToThrow(additionPatch);
  expectValidPatchNotToThrow(binaryPatch);

  expectInvalidPatchToThrow(noFilesPatch, "Invalid patch: no changes found");
});

function expectValidPatchNotToThrow(patch: string) {
  it("should not throw for valid patch", async () => {
    await expect(assertValidPatch(patch)).resolves.not.toThrow();
  });
}

function expectInvalidPatchToThrow(patch: string, errorMessage: string) {
  it(`should throw for invalid patch: ${errorMessage}`, async () => {
    await expect(assertValidPatch(patch)).rejects.toThrow(errorMessage);
  });
}
