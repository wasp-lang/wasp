import assert from "node:assert/strict";
import test from "node:test";
import { MAX_REVIEW_COMMENT_LENGTH, REVIEW_MARKER } from "../config.ts";
import {
  selectCodeReviewThreads,
  serializeReviewContextForCodex,
} from "../review-context.ts";
import type { ReviewContext, ReviewThread } from "../schema.ts";

function reviewThread(authorLogin: string, body: string): ReviewThread {
  return {
    id: `${authorLogin}-${body}`,
    isResolved: false,
    isOutdated: false,
    path: "src/example.ts",
    line: 1,
    startLine: null,
    viewerCanResolve: true,
    comments: [
      {
        id: "comment-1",
        author: { login: authorLogin },
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

test("bounds comment history and body length in the Codex context", () => {
  const comments = Array.from({ length: 25 }, (_, index) => ({
    id: `comment-${index}`,
    author: { login: "github-actions[bot]" },
    body: index === 24 ? "x".repeat(MAX_REVIEW_COMMENT_LENGTH + 1) : `${index}`,
  }));
  const unresolvedThread = {
    ...reviewThread("github-actions[bot]", REVIEW_MARKER),
    comments,
  };
  const resolvedThread = {
    ...unresolvedThread,
    id: "resolved-thread",
    isResolved: true,
  };
  const serializedContext = serializeReviewContextForCodex(
    reviewContext([unresolvedThread, resolvedThread]),
  );
  const parsedContext = JSON.parse(serializedContext) as ReviewContext;

  assert.deepEqual(
    parsedContext.reviewThreads[0].comments.map(({ id }) => id),
    [
      "comment-0",
      ...Array.from({ length: 20 }, (_, index) => `comment-${index + 5}`),
    ],
  );
  assert.equal(
    parsedContext.reviewThreads[0].comments.at(-1)?.body.length,
    MAX_REVIEW_COMMENT_LENGTH,
  );
  assert.deepEqual(
    parsedContext.reviewThreads[1].comments.map(({ id }) => id),
    ["comment-0"],
  );
});

test("rejects a Codex context above the byte limit", () => {
  const largeThreads = Array.from({ length: 140 }, (_, index) => ({
    ...reviewThread("github-actions[bot]", REVIEW_MARKER),
    id: `thread-${index}`,
    comments: [
      {
        id: `comment-${index}`,
        author: { login: "github-actions[bot]" },
        body: "x".repeat(MAX_REVIEW_COMMENT_LENGTH),
      },
    ],
  }));

  assert.throws(
    () => serializeReviewContextForCodex(reviewContext(largeThreads)),
    /maximum is/,
  );
});

function reviewContext(reviewThreads: ReviewThread[]): ReviewContext {
  return {
    repository: { owner: "wasp-lang", name: "wasp" },
    pullRequest: {
      number: 42,
      baseSha: "a".repeat(40),
      headSha: "b".repeat(40),
      state: "OPEN",
      isDraft: false,
    },
    reviewerLogin: "github-actions[bot]",
    reviewThreads,
  };
}
