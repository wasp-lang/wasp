import assert from "node:assert/strict";
import test from "node:test";
import { ReviewSchema } from "./schema.mjs";

const validReview = {
  summary: "No issues found.",
  findings: [
    {
      title: "Finding",
      body: "Description",
      severity: "WARNING",
      path: "src/example.ts",
      startLine: 1,
      endLine: 2,
    },
  ],
};

test("rejects unsafe paths", () => {
  assert.throws(() =>
    ReviewSchema.parse(reviewWith({ path: "../secret.txt" })),
  );
});

test("rejects an end line before the start line", () => {
  assert.throws(() =>
    ReviewSchema.parse(reviewWith({ startLine: 2, endLine: 1 })),
  );
});

function reviewWith(findingOverrides) {
  return {
    ...validReview,
    findings: [{ ...validReview.findings[0], ...findingOverrides }],
  };
}
