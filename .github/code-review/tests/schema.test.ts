import assert from "node:assert/strict";
import test from "node:test";
import { ReviewSchema } from "../schema.ts";

const validReview = {
  summary: "No issues found.",
  threadsToResolve: [],
  newFindings: [
    {
      title: "Finding",
      body: "If the value is false, this returns the wrong result.",
      suggestion: null,
      path: "src/example.ts",
      startLine: 1,
      endLine: 2,
    },
  ],
};

test("rejects unsafe paths", () => {
  assert.throws(() =>
    ReviewSchema.parse({
      ...validReview,
      newFindings: [{ ...validReview.newFindings[0], path: "../secret.txt" }],
    }),
  );
});

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

test("rejects duplicate resolutions for an existing thread", () => {
  assert.throws(() =>
    ReviewSchema.parse({
      ...validReview,
      threadsToResolve: [
        {
          threadId: "thread-1",
          lastCommentId: "comment-1",
        },
        {
          threadId: "thread-1",
          lastCommentId: "comment-1",
        },
      ],
    }),
  );
});
