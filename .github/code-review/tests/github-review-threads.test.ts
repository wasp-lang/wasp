import assert from "node:assert/strict";
import test from "node:test";
import { fetchReviewThread } from "../github-review-threads.ts";
import type { GitHubOctokit } from "../github.ts";

const firstComment = {
  id: "comment-1",
  author: { login: "github-actions[bot]" },
  body: "Original finding",
};
const latestComment = {
  id: "comment-2",
  author: { login: "developer" },
  body: "Fixed",
};

test("merges the original and recent comments without duplicates", async () => {
  const thread = await fetchReviewThread(
    octokitReturning({
      isResolved: false,
      comments: { nodes: [firstComment] },
      recentComments: { nodes: [firstComment, latestComment] },
    }),
    "thread-1",
  );

  assert.deepEqual(thread.comments, [firstComment, latestComment]);
  assert.equal(thread.comments.at(-1)?.id, latestComment.id);
});

test("keeps only the original comment for a resolved thread", async () => {
  const thread = await fetchReviewThread(
    octokitReturning({
      isResolved: true,
      comments: { nodes: [firstComment] },
      recentComments: { nodes: [latestComment] },
    }),
    "thread-1",
  );

  assert.deepEqual(thread.comments, [firstComment]);
});

function octokitReturning({
  isResolved,
  comments,
  recentComments,
}: {
  isResolved: boolean;
  comments: { nodes: (typeof firstComment)[] };
  recentComments: { nodes: (typeof firstComment)[] };
}): GitHubOctokit {
  return {
    graphql: async () => ({
      node: {
        id: "thread-1",
        isResolved,
        isOutdated: false,
        path: "src/example.ts",
        line: 1,
        startLine: null,
        viewerCanResolve: true,
        comments,
        recentComments,
      },
    }),
  } as unknown as GitHubOctokit;
}
