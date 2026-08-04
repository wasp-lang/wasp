import { REVIEW_MARKER } from "./config.ts";
import { fetchReviewSnapshot } from "./github-review-threads.ts";
import type { GitHubOctokit } from "./github.ts";
import {
  PullRequestSchema,
  ReviewContextSchema,
  type Repository,
  type ReviewContext,
  type ReviewThread,
} from "./schema.ts";

export async function loadReviewContext({
  octokit,
  repository,
  pullNumber,
  expectedBaseSha,
  expectedHeadSha,
}: {
  octokit: GitHubOctokit;
  repository: Repository;
  pullNumber: number;
  expectedBaseSha: string;
  expectedHeadSha: string;
}): Promise<ReviewContext> {
  const [{ data: pullRequestResponse }, reviewSnapshot] = await Promise.all([
    octokit.rest.pulls.get({
      owner: repository.owner,
      repo: repository.name,
      pull_number: pullNumber,
    }),
    fetchReviewSnapshot(octokit, repository, pullNumber),
  ]);
  const pullRequest = PullRequestSchema.parse({
    number: pullRequestResponse.number,
    baseSha: pullRequestResponse.base.sha,
    headSha: pullRequestResponse.head.sha,
    state: pullRequestResponse.merged
      ? "MERGED"
      : pullRequestResponse.state.toUpperCase(),
    isDraft: pullRequestResponse.draft ?? false,
  });

  if (
    pullRequest.baseSha !== expectedBaseSha ||
    pullRequest.headSha !== expectedHeadSha
  ) {
    throw new Error(
      `Pull request range changed from ${expectedBaseSha}...${expectedHeadSha} to ${pullRequest.baseSha}...${pullRequest.headSha}.`,
    );
  }
  if (pullRequest.state !== "OPEN" || pullRequest.isDraft) {
    throw new Error(`Pull request #${pullNumber} is not ready for review.`);
  }

  return ReviewContextSchema.parse({
    repository,
    pullRequest,
    reviewerLogin: reviewSnapshot.reviewerLogin,
    reviewThreads: selectCodeReviewThreads(
      reviewSnapshot.reviewThreads,
      reviewSnapshot.reviewerLogin,
    ),
  });
}

export function selectCodeReviewThreads(
  reviewThreads: ReviewThread[],
  reviewerLogin: string,
): ReviewThread[] {
  return reviewThreads.filter(
    (thread) =>
      thread.comments[0]?.author?.login === reviewerLogin &&
      thread.comments[0].body.includes(REVIEW_MARKER),
  );
}
