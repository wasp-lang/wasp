import assert from "node:assert/strict";
import test from "node:test";
import { ReviewSchema } from "../schema.ts";

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

test("rejects an end line before the start line", () => {
  assert.throws(() =>
    ReviewSchema.parse({
      ...validReview,
      newFindings: [
        { ...validReview.newFindings[0], startLine: 2, endLine: 1 },
      ],
    }),
  );
});

test("preserves code suggestion indentation", () => {
  const review = ReviewSchema.parse({
    ...validReview,
    newFindings: [
      { ...validReview.newFindings[0], suggestion: "  const value = true;" },
    ],
  });

  assert.equal(review.newFindings[0].suggestion, "  const value = true;");
});
