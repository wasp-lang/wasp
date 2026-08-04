import assert from "node:assert/strict";
import test from "node:test";
import { publishCodeReview } from "../review-publisher.ts";
import type { CodexReview, PullRequest, ReviewThread } from "../schema.ts";

const repository = { owner: "wasp-lang", name: "wasp" };
const expectedHeadSha = "b".repeat(40);
const pullRequest: PullRequest = {
  number: 42,
  baseSha: "a".repeat(40),
  headSha: expectedHeadSha,
  state: "OPEN",
  isDraft: false,
};
const reviewThread: ReviewThread = {
  id: "thread-1",
  isResolved: false,
  isOutdated: false,
  path: "src/example.ts",
  line: 2,
  startLine: null,
  canResolve: true,
  comments: [
    {
      id: "comment-1",
      authorLogin: "github-actions[bot]",
      body: "<!-- wasp-code-review -->\nFinding",
    },
  ],
};
const codexReview: CodexReview = {
  summary: "Review complete.",
  existingThreadDecisions: [
    {
      threadId: reviewThread.id,
      lastCommentId: "comment-1",
      disposition: "resolve",
    },
  ],
  newFindings: [],
};

test("publishes nothing when the pull request head changes", async () => {
  let pullRequestFetches = 0;
  let writes = 0;
  const github = githubStub({
    fetchPullRequest: async () => {
      pullRequestFetches += 1;
      return pullRequestFetches === 1
        ? pullRequest
        : { ...pullRequest, headSha: "c".repeat(40) };
    },
    resolveReviewThread: async () => {
      writes += 1;
    },
    createOrUpdateReviewSummary: async () => {
      writes += 1;
    },
  });

  await assert.rejects(
    publishCodeReview({
      github,
      repository,
      pullNumber: 42,
      expectedHeadSha,
      codexReview,
    }),
    /head changed/,
  );
  assert.equal(writes, 0);
});

test("publishes nothing when a thread receives a new reply", async () => {
  let writes = 0;
  const github = githubStub({
    fetchReviewThread: async () => ({
      ...reviewThread,
      comments: [
        ...reviewThread.comments,
        {
          id: "comment-2",
          authorLogin: "developer",
          body: "One more detail.",
        },
      ],
    }),
    resolveReviewThread: async () => {
      writes += 1;
    },
    createOrUpdateReviewSummary: async () => {
      writes += 1;
    },
  });

  await assert.rejects(
    publishCodeReview({
      github,
      repository,
      pullNumber: 42,
      expectedHeadSha,
      codexReview,
    }),
    /changed while the review was being published/,
  );
  assert.equal(writes, 0);
});

function githubStub(overrides: Record<string, unknown> = {}) {
  return {
    fetchPullRequest: async () => pullRequest,
    fetchPullRequestDiff: async () => "",
    fetchReviewSnapshot: async () => ({
      reviewerLogin: "github-actions[bot]",
      reviewThreads: [reviewThread],
    }),
    fetchReviewThread: async () => reviewThread,
    submitPullRequestReview: async () => {},
    resolveReviewThread: async () => {},
    createOrUpdateReviewSummary: async () => {},
    ...overrides,
  };
}
