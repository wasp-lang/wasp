import assert from "node:assert/strict";
import test from "node:test";
import { ReviewSchema } from "../schema.ts";

const validReview = {
  summary: "No issues found.",
  existingThreadDecisions: [],
  newFindings: [
    {
      title: "Finding",
      body: "Description",
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

test("rejects duplicate decisions for an existing thread", () => {
  assert.throws(() =>
    ReviewSchema.parse({
      ...validReview,
      existingThreadDecisions: [
        {
          threadId: "thread-1",
          lastCommentId: "comment-1",
          disposition: "keep",
        },
        {
          threadId: "thread-1",
          lastCommentId: "comment-1",
          disposition: "resolve",
        },
      ],
    }),
  );
});
