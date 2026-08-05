import assert from "node:assert/strict";
import test from "node:test";
import { REVIEW_SUMMARY_MARKER } from "../config.ts";
import { formatReviewSummary } from "../review-publisher.ts";
import type { NewFinding } from "../schema.ts";

const repository = { owner: "wasp-lang", name: "wasp" };
const reviewedHeadSha = "a".repeat(40);
const commitUrl = `https://github.com/wasp-lang/wasp/commit/${reviewedHeadSha}`;
const publishedFindings: NewFinding[] = [
  {
    body: "First finding",
    suggestion: "const first = true;",
    path: "src/example.ts",
    startLine: 1,
    endLine: 1,
  },
  {
    body: "Second finding",
    suggestion: "const second = true;",
    path: "src/example.ts",
    startLine: 2,
    endLine: 2,
  },
];

test("reports comments, resolutions, files, and suggestions", () => {
  assert.equal(
    formatReviewSummary({
      repository,
      reviewedHeadSha,
      publishedFindings,
      resolvedThreadCount: 1,
      modelUsed: "gpt-5.6-sol",
    }),
    `${REVIEW_SUMMARY_MARKER}\n## Latest code review status\n\nPosted **2 comments** and resolved **1 thread**. Comments cover **1 file** and include **2 suggested changes**.\n\nModel used: \`gpt-5.6-sol\`.\n\nReviewed commit [\`aaaaaaa\`](${commitUrl}).\n`,
  );
});

test("omits empty activity details", () => {
  assert.equal(
    formatReviewSummary({
      repository,
      reviewedHeadSha,
      publishedFindings: [],
      resolvedThreadCount: 0,
      modelUsed: "gpt-5.6-sol",
    }),
    `${REVIEW_SUMMARY_MARKER}\n## Latest code review status\n\nNo new comments or thread updates.\n\nModel used: \`gpt-5.6-sol\`.\n\nReviewed commit [\`aaaaaaa\`](${commitUrl}).\n`,
  );
});
