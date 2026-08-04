import assert from "node:assert/strict";
import test from "node:test";
import { REVIEW_MARKER } from "../config.ts";
import { selectCodeReviewThreads } from "../review-context.ts";
import type { ReviewThread } from "../schema.ts";

function reviewThread(authorLogin: string, body: string): ReviewThread {
  return {
    id: `${authorLogin}-${body}`,
    isResolved: false,
    isOutdated: false,
    path: "src/example.ts",
    line: 1,
    startLine: null,
    canResolve: true,
    comments: [
      {
        id: "comment-1",
        authorLogin,
        body,
      },
    ],
  };
}

test("selects only marker-owned threads created by the reviewer", () => {
  const owned = reviewThread("github-actions[bot]", REVIEW_MARKER);
  const copiedMarker = reviewThread("developer", REVIEW_MARKER);
  const unmarked = reviewThread("github-actions[bot]", "Another review");

  assert.deepEqual(
    selectCodeReviewThreads(
      [owned, copiedMarker, unmarked],
      "github-actions[bot]",
    ),
    [owned],
  );
});
