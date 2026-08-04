import assert from "node:assert/strict";
import test from "node:test";
import { createGitHubClient } from "../github-api.ts";

const repository = { owner: "wasp-lang", name: "wasp" };

test("submits multiline findings as one pull request review", async () => {
  let requestBody: unknown;
  const github = createGitHubClient({
    token: "token",
    fetch: (async (_input, init) => {
      requestBody = JSON.parse(String(init?.body)) as unknown;
      return new Response("{}", { status: 200 });
    }) as typeof fetch,
  });

  await github.submitPullRequestReview({
    repository,
    pullNumber: 42,
    commitSha: "a".repeat(40),
    body: "Review",
    comments: [
      {
        path: "src/example.ts",
        body: "Finding",
        startLine: 2,
        endLine: 4,
      },
    ],
  });

  assert.deepEqual(requestBody, {
    commit_id: "a".repeat(40),
    event: "COMMENT",
    body: "Review",
    comments: [
      {
        path: "src/example.ts",
        body: "Finding",
        line: 4,
        side: "RIGHT",
        start_line: 2,
        start_side: "RIGHT",
      },
    ],
  });
});

test("gets the review identity from GraphQL viewer", async () => {
  const github = createGitHubClient({
    token: "installation-token",
    fetch: (async (input) => {
      assert.equal(String(input), "https://api.github.com/graphql");
      return Response.json({
        data: {
          viewer: { login: "github-actions[bot]" },
          repository: {
            pullRequest: {
              reviewThreads: {
                nodes: [],
                pageInfo: { endCursor: null, hasNextPage: false },
              },
            },
          },
        },
      });
    }) as typeof fetch,
  });

  assert.deepEqual(await github.fetchReviewSnapshot(repository, 42), {
    reviewerLogin: "github-actions[bot]",
    reviewThreads: [],
  });
});
