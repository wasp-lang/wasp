import { REVIEW_MARKER, REVIEW_SUMMARY_MARKER } from "./config.ts";
import type { GitHubClient } from "./github-api.ts";
import {
  buildPublicationPlan,
  formatFindingComment,
} from "./publication-plan.ts";
import { loadReviewContext } from "./review-context.ts";
import type {
  CodexReview,
  InlineReviewComment,
  PublicationPlan,
  Repository,
} from "./schema.ts";

type PublisherGitHub = Pick<
  GitHubClient,
  | "createOrUpdateReviewSummary"
  | "fetchPullRequest"
  | "fetchPullRequestDiff"
  | "fetchReviewSnapshot"
  | "fetchReviewThread"
  | "resolveReviewThread"
  | "submitPullRequestReview"
>;

export async function publishCodeReview({
  github,
  repository,
  pullNumber,
  expectedHeadSha,
  codexReview,
}: {
  github: PublisherGitHub;
  repository: Repository;
  pullNumber: number;
  expectedHeadSha: string;
  codexReview: CodexReview;
}): Promise<void> {
  const pullRequestDiff = await github.fetchPullRequestDiff(
    repository,
    pullNumber,
  );
  const reviewContext = await loadReviewContext({
    github,
    repository,
    pullNumber,
    expectedHeadSha,
  });
  const publicationPlan = buildPublicationPlan({
    reviewContext,
    codexReview,
    pullRequestDiff,
  });

  await assertPullRequestHeadIsUnchanged({
    github,
    repository,
    pullNumber,
    expectedHeadSha,
  });
  await resolveAddressedThreads({ github, publicationPlan });
  await publishNewFindings({ github, repository, pullNumber, publicationPlan });
  await github.createOrUpdateReviewSummary({
    repository,
    pullNumber,
    marker: REVIEW_SUMMARY_MARKER,
    reviewerLogin: reviewContext.reviewerLogin,
    body: formatReviewSummary(publicationPlan),
  });
}

async function assertPullRequestHeadIsUnchanged({
  github,
  repository,
  pullNumber,
  expectedHeadSha,
}: {
  github: PublisherGitHub;
  repository: Repository;
  pullNumber: number;
  expectedHeadSha: string;
}): Promise<void> {
  const pullRequest = await github.fetchPullRequest(repository, pullNumber);
  if (pullRequest.headSha !== expectedHeadSha) {
    throw new Error(
      `Pull request head changed from ${expectedHeadSha} to ${pullRequest.headSha}; no review was published.`,
    );
  }
}

async function publishNewFindings({
  github,
  repository,
  pullNumber,
  publicationPlan,
}: {
  github: PublisherGitHub;
  repository: Repository;
  pullNumber: number;
  publicationPlan: PublicationPlan;
}): Promise<void> {
  if (publicationPlan.newFindings.length === 0) return;

  const comments: InlineReviewComment[] = publicationPlan.newFindings.map(
    (finding) => ({
      path: finding.path,
      startLine: finding.startLine,
      endLine: finding.endLine,
      body: formatFindingComment(finding, publicationPlan.reviewedHeadSha),
    }),
  );

  await github.submitPullRequestReview({
    repository,
    pullNumber,
    commitSha: publicationPlan.reviewedHeadSha,
    body: `${REVIEW_MARKER}\nAutomated code review findings.`,
    comments,
  });
}

async function resolveAddressedThreads({
  github,
  publicationPlan,
}: {
  github: PublisherGitHub;
  publicationPlan: PublicationPlan;
}): Promise<void> {
  for (const { threadId, lastCommentId } of publicationPlan.threadsToResolve) {
    const thread = await github.fetchReviewThread(threadId);
    if (thread.isResolved) continue;
    assertThreadVersion(thread, lastCommentId);
    await github.resolveReviewThread(threadId);
  }
}

function assertThreadVersion(
  thread: Awaited<ReturnType<PublisherGitHub["fetchReviewThread"]>>,
  expectedLastCommentId: string,
): void {
  if (thread.comments.at(-1)?.id !== expectedLastCommentId) {
    throw new Error(
      `Review thread ${thread.id} changed while the review was being published.`,
    );
  }
  if (!thread.canResolve) {
    throw new Error(
      `Review thread ${thread.id} cannot be resolved by this token.`,
    );
  }
}

function formatReviewSummary(publicationPlan: PublicationPlan): string {
  const newCount = publicationPlan.newFindings.length;
  const openCount = publicationPlan.threadIdsToKeep.length + newCount;
  const resolvedCount = publicationPlan.threadsToResolve.length;

  return `${REVIEW_SUMMARY_MARKER}
## Code review

${publicationPlan.summary}

| New | Open | Resolved |
| ---: | ---: | -------: |
| ${newCount} | ${openCount} | ${resolvedCount} |

Reviewed commit \`${publicationPlan.reviewedHeadSha}\`.
`;
}
