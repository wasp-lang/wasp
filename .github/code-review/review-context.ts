import { REVIEW_MARKER } from "./config.ts";
import type { GitHubClient } from "./github-api.ts";
import {
  ReviewContextSchema,
  type Repository,
  type ReviewContext,
  type ReviewThread,
} from "./schema.ts";

export type ReviewContextGitHub = Pick<
  GitHubClient,
  "fetchPullRequest" | "fetchReviewSnapshot"
>;

export async function loadReviewContext({
  github,
  repository,
  pullNumber,
  expectedHeadSha,
}: {
  github: ReviewContextGitHub;
  repository: Repository;
  pullNumber: number;
  expectedHeadSha: string;
}): Promise<ReviewContext> {
  const [pullRequest, reviewSnapshot] = await Promise.all([
    github.fetchPullRequest(repository, pullNumber),
    github.fetchReviewSnapshot(repository, pullNumber),
  ]);

  if (pullRequest.headSha !== expectedHeadSha) {
    throw new Error(
      `Pull request head changed from ${expectedHeadSha} to ${pullRequest.headSha}.`,
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
      thread.comments[0]?.authorLogin === reviewerLogin &&
      thread.comments[0].body.includes(REVIEW_MARKER),
  );
}
