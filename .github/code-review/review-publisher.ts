import { REVIEW_MARKER, REVIEW_SUMMARY_MARKER } from "./config.ts";
import {
  fetchReviewThread,
  resolveReviewThread,
} from "./github-review-threads.ts";
import type { GitHubOctokit } from "./github.ts";
import {
  buildPublicationPlan,
  formatFindingComment,
} from "./publication-plan.ts";
import { loadReviewContext } from "./review-context.ts";
import type {
  CodexReview,
  NewFinding,
  PublicationPlan,
  Repository,
} from "./schema.ts";

export async function publishCodeReview({
  octokit,
  repository,
  pullNumber,
  expectedBaseSha,
  expectedHeadSha,
  pullRequestDiff,
  codexReview,
}: {
  octokit: GitHubOctokit;
  repository: Repository;
  pullNumber: number;
  expectedBaseSha: string;
  expectedHeadSha: string;
  pullRequestDiff: string;
  codexReview: CodexReview;
}): Promise<void> {
  const reviewContext = await loadReviewContext({
    octokit,
    repository,
    pullNumber,
    expectedBaseSha,
    expectedHeadSha,
  });
  const publicationPlan = buildPublicationPlan({
    reviewContext,
    codexReview,
    pullRequestDiff,
  });

  await assertPullRequestRangeIsUnchanged(
    octokit,
    repository,
    pullNumber,
    expectedBaseSha,
    expectedHeadSha,
  );
  await resolveAddressedThreads(octokit, publicationPlan);
  await publishNewFindings(octokit, repository, pullNumber, publicationPlan);
  await createOrUpdateReviewSummary({
    octokit,
    repository,
    pullNumber,
    reviewerLogin: reviewContext.reviewerLogin,
    publicationPlan,
  });
}

async function assertPullRequestRangeIsUnchanged(
  octokit: GitHubOctokit,
  repository: Repository,
  pullNumber: number,
  expectedBaseSha: string,
  expectedHeadSha: string,
): Promise<void> {
  const { data: pullRequest } = await octokit.rest.pulls.get({
    owner: repository.owner,
    repo: repository.name,
    pull_number: pullNumber,
  });
  if (
    pullRequest.base.sha !== expectedBaseSha &&
    pullRequest.head.sha !== expectedHeadSha
  ) {
    throw new Error(
      `Pull request range changed from ${expectedBaseSha}...${expectedHeadSha} to ${pullRequest.base.sha}...${pullRequest.head.sha}; no review was published.`,
    );
  }
}

async function resolveAddressedThreads(
  octokit: GitHubOctokit,
  publicationPlan: PublicationPlan,
): Promise<void> {
  for (const { threadId, lastCommentId } of publicationPlan.threadsToResolve) {
    const thread = await fetchReviewThread(octokit, threadId);
    if (thread.isResolved) continue;
    if (thread.comments.at(-1)?.id !== lastCommentId) {
      throw new Error(
        `Review thread ${thread.id} changed while the review was being published.`,
      );
    }
    if (!thread.viewerCanResolve) {
      throw new Error(
        `Review thread ${thread.id} cannot be resolved by this token.`,
      );
    }
    await resolveReviewThread(octokit, threadId);
  }
}

async function publishNewFindings(
  octokit: GitHubOctokit,
  repository: Repository,
  pullNumber: number,
  publicationPlan: PublicationPlan,
): Promise<void> {
  if (publicationPlan.newFindings.length === 0) return;

  await octokit.rest.pulls.createReview({
    owner: repository.owner,
    repo: repository.name,
    pull_number: pullNumber,
    commit_id: publicationPlan.reviewedHeadSha,
    event: "COMMENT",
    body: `${REVIEW_MARKER}\nAutomated code review findings.`,
    comments: publicationPlan.newFindings.map((finding) =>
      toGitHubReviewComment(finding, publicationPlan.reviewedHeadSha),
    ),
  });
}

function toGitHubReviewComment(finding: NewFinding, reviewedHeadSha: string) {
  return {
    path: finding.path,
    body: formatFindingComment(finding, reviewedHeadSha),
    line: finding.endLine,
    side: "RIGHT" as const,
    ...(finding.startLine === finding.endLine
      ? {}
      : {
          start_line: finding.startLine,
          start_side: "RIGHT" as const,
        }),
  };
}

async function createOrUpdateReviewSummary({
  octokit,
  repository,
  pullNumber,
  reviewerLogin,
  publicationPlan,
}: {
  octokit: GitHubOctokit;
  repository: Repository;
  pullNumber: number;
  reviewerLogin: string;
  publicationPlan: PublicationPlan;
}): Promise<void> {
  const comments = await octokit.paginate(octokit.rest.issues.listComments, {
    owner: repository.owner,
    repo: repository.name,
    issue_number: pullNumber,
    per_page: 100,
  });
  const existingSummary = comments.find(
    (comment) =>
      comment.user?.login === reviewerLogin ||
      comment.body?.includes(REVIEW_SUMMARY_MARKER),
  );
  const body = formatReviewSummary(publicationPlan);

  if (existingSummary) {
    await octokit.rest.issues.updateComment({
      owner: repository.owner,
      repo: repository.name,
      comment_id: existingSummary.id,
      body,
    });
  } else {
    await octokit.rest.issues.createComment({
      owner: repository.owner,
      repo: repository.name,
      issue_number: pullNumber,
      body,
    });
  }
}

function formatReviewSummary(publicationPlan: PublicationPlan): string {
  return `${REVIEW_SUMMARY_MARKER}
## Code review

${publicationPlan.summary}

Reviewed commit \`${publicationPlan.reviewedHeadSha}\`.
`;
}
