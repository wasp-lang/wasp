import assert from "node:assert/strict";
import test from "node:test";
import { REVIEW_MARKER } from "../config.ts";
import {
  buildPublicationPlan,
  formatFindingComment,
} from "../publication-plan.ts";
import type {
  CodexReview,
  NewFinding,
  ReviewContext,
  ReviewThread,
} from "../schema.ts";

const finding: NewFinding = {
  title: "Finding",
  body: "Description",
  path: "src/example.ts",
  startLine: 2,
  endLine: 2,
};

const unresolvedThread: ReviewThread = {
  id: "thread-1",
  isResolved: false,
  isOutdated: false,
  path: "src/old.ts",
  line: 3,
  startLine: null,
  viewerCanResolve: true,
  comments: [
    {
      id: "comment-1",
      author: { login: "github-actions[bot]" },
      body: `${REVIEW_MARKER}\nOriginal finding`,
    },
    {
      id: "comment-2",
      author: { login: "developer" },
      body: "Fixed.",
    },
  ],
};

const reviewContext: ReviewContext = {
  repository: { owner: "wasp-lang", name: "wasp" },
  pullRequest: {
    number: 42,
    baseSha: "a".repeat(40),
    headSha: "b".repeat(40),
    state: "OPEN",
    isDraft: false,
  },
  reviewerLogin: "github-actions[bot]",
  reviewThreads: [unresolvedThread],
};

const codexReview: CodexReview = {
  summary: "One issue remains.",
  existingThreadDecisions: [
    {
      threadId: unresolvedThread.id,
      lastCommentId: "comment-2",
      disposition: "resolve",
    },
  ],
  newFindings: [finding],
};

const pullRequestDiff = `diff --git a/src/example.ts b/src/example.ts
--- a/src/example.ts
+++ b/src/example.ts
@@ -1 +1,2 @@
 const before = true;
+const added = true;
`;

test("builds additions, keeps, and resolutions from one review snapshot", () => {
  const plan = buildPublicationPlan({
    reviewContext,
    codexReview,
    pullRequestDiff,
  });

  assert.deepEqual(plan.newFindings, [finding]);
  assert.deepEqual(plan.threadIdsToKeep, []);
  assert.deepEqual(plan.threadsToResolve, [
    { threadId: unresolvedThread.id, lastCommentId: "comment-2" },
  ]);
  assert.equal(plan.reviewedHeadSha, reviewContext.pullRequest.headSha);
});

test("rejects a decision made before the latest thread reply", () => {
  assert.throws(
    () =>
      buildPublicationPlan({
        reviewContext,
        codexReview: {
          ...codexReview,
          existingThreadDecisions: [
            {
              ...codexReview.existingThreadDecisions[0],
              lastCommentId: "comment-1",
            },
          ],
        },
        pullRequestDiff,
      }),
    /changed while the review was running/,
  );
});

test("omits a finding already published by this reviewer", () => {
  const resolvedDuplicate: ReviewThread = {
    ...unresolvedThread,
    id: "thread-2",
    isResolved: true,
    comments: [
      {
        ...unresolvedThread.comments[0],
        id: "comment-3",
        body: formatFindingComment(finding, reviewContext.pullRequest.headSha),
      },
    ],
  };

  const plan = buildPublicationPlan({
    reviewContext: {
      ...reviewContext,
      reviewThreads: [unresolvedThread, resolvedDuplicate],
    },
    codexReview,
    pullRequestDiff,
  });

  assert.deepEqual(plan.newFindings, []);
});

test("retries after publishing findings without requiring new decisions", () => {
  const partiallyPublishedFinding: ReviewThread = {
    ...unresolvedThread,
    id: "thread-2",
    comments: [
      {
        ...unresolvedThread.comments[0],
        id: "comment-3",
        body: formatFindingComment(finding, reviewContext.pullRequest.headSha),
      },
    ],
  };

  const plan = buildPublicationPlan({
    reviewContext: {
      ...reviewContext,
      reviewThreads: [unresolvedThread, partiallyPublishedFinding],
    },
    codexReview,
    pullRequestDiff,
  });

  assert.deepEqual(plan.newFindings, []);
  assert.equal(plan.newFindingCount, 1);
  assert.deepEqual(plan.threadsToResolve, [
    { threadId: unresolvedThread.id, lastCommentId: "comment-2" },
  ]);
});

test("allows the same finding on a later commit", () => {
  const previousHeadSha = "c".repeat(40);
  const resolvedPreviousFinding: ReviewThread = {
    ...unresolvedThread,
    id: "thread-2",
    isResolved: true,
    comments: [
      {
        ...unresolvedThread.comments[0],
        id: "comment-3",
        body: formatFindingComment(finding, previousHeadSha),
      },
    ],
  };

  const plan = buildPublicationPlan({
    reviewContext: {
      ...reviewContext,
      reviewThreads: [unresolvedThread, resolvedPreviousFinding],
    },
    codexReview,
    pullRequestDiff,
  });

  assert.deepEqual(plan.newFindings, [finding]);
});

test("ignores a decision for a thread resolved while Codex was running", () => {
  const plan = buildPublicationPlan({
    reviewContext: {
      ...reviewContext,
      reviewThreads: [{ ...unresolvedThread, isResolved: true }],
    },
    codexReview,
    pullRequestDiff,
  });

  assert.deepEqual(plan.threadIdsToKeep, []);
  assert.deepEqual(plan.threadsToResolve, []);
  assert.deepEqual(plan.newFindings, [finding]);
});
