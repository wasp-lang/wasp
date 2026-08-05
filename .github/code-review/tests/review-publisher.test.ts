import assert from "node:assert/strict";
import test from "node:test";
import { REVIEW_SUMMARY_MARKER } from "../config.ts";
import { formatReviewSummary } from "../review-publisher.ts";

test("reports completed publication activity", () => {
  assert.equal(
    formatReviewSummary({
      reviewedHeadSha: "a".repeat(40),
      postedCommentCount: 2,
      resolvedThreadCount: 1,
    }),
    `${REVIEW_SUMMARY_MARKER}\n## Code review\n\nFinished review of \`${"a".repeat(40)}\`: posted 2 comments and resolved 1 thread.\n`,
  );
});
