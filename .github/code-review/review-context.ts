import {
  MAX_REVIEW_COMMENT_LENGTH,
  MAX_REVIEW_CONTEXT_BYTES,
  REVIEW_MARKER,
} from "./config.ts";
import { fetchReviewSnapshot } from "./github-review-threads.ts";
import type { GitHubOctokit } from "./github.ts";
import {
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
  const currentBaseSha = pullRequestResponse.base.sha;
  const currentHeadSha = pullRequestResponse.head.sha;

  if (
    currentBaseSha !== expectedBaseSha ||
    currentHeadSha !== expectedHeadSha
  ) {
    throw new Error(
      `Pull request range changed from ${expectedBaseSha}...${expectedHeadSha} to ${currentBaseSha}...${currentHeadSha}.`,
    );
  }
  if (
    pullRequestResponse.state !== "open" ||
    pullRequestResponse.merged ||
    pullRequestResponse.draft
  ) {
    throw new Error(`Pull request #${pullNumber} is not ready for review.`);
  }

  return ReviewContextSchema.parse({
    reviewedHeadSha: currentHeadSha,
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

export function serializeReviewContextForCodex(
  reviewContext: ReviewContext,
): string {
  const boundedContext: ReviewContext = {
    ...reviewContext,
    reviewThreads: reviewContext.reviewThreads.map((thread) => ({
      ...thread,
      comments: thread.comments.map((comment) => ({
        ...comment,
        body: truncateComment(comment.body),
      })),
    })),
  };
  const serializedContext = `${JSON.stringify(boundedContext, null, 2)}\n`;
  const contextSize = Buffer.byteLength(serializedContext);

  if (contextSize > MAX_REVIEW_CONTEXT_BYTES) {
    throw new Error(
      `Review context is ${contextSize} bytes; maximum is ${MAX_REVIEW_CONTEXT_BYTES}.`,
    );
  }

  return serializedContext;
}

function truncateComment(body: string): string {
  const suffix = "\n[truncated]";
  if (body.length <= MAX_REVIEW_COMMENT_LENGTH) return body;
  return `${body.slice(0, MAX_REVIEW_COMMENT_LENGTH - suffix.length)}${suffix}`;
}
