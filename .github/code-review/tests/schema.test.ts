import assert from "node:assert/strict";
import test from "node:test";
import { CodexOutputSchema } from "../schema.ts";

const validReview = {
  threadsToResolve: [],
  newFindings: [
    {
      body: "If the value is false, this returns the wrong result.",
      suggestion: null,
      path: "src/example.ts",
      startLine: 1,
      endLine: 2,
    },
  ],
};

test("preserves code suggestion indentation", () => {
  const review = CodexOutputSchema.parse({
    ...validReview,
    newFindings: [
      { ...validReview.newFindings[0], suggestion: "  const value = true;" },
    ],
  });

  assert.equal(review.newFindings[0].suggestion, "  const value = true;");
});
