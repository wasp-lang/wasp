import crypto from "node:crypto";
import { REVIEW_MARKER } from "./config.ts";
import {
  isFindingOnChangedLines,
  parsePullRequestDiff,
} from "./pull-request-diff.ts";
import {
  type CodexReview,
  type ExistingThreadDecision,
  type NewFinding,
  type PublicationPlan,
  type ReviewContext,
  type ReviewThread,
} from "./schema.ts";

const FINGERPRINT_PATTERN =
  /<!-- wasp-code-review:fingerprint=([0-9a-f]{64}) -->/;

export function buildPublicationPlan({
  reviewContext,
  codexReview,
  pullRequestDiff,
}: {
  reviewContext: ReviewContext;
  codexReview: CodexReview;
  pullRequestDiff: string;
}): PublicationPlan {
  const proposedFingerprints = new Set(
    codexReview.newFindings.map((finding) =>
      fingerprintFinding(finding, reviewContext.pullRequest.headSha),
    ),
  );
  const threadsRequiringDecision = reviewContext.reviewThreads.filter(
    (thread) => !proposedFingerprints.has(findThreadFingerprint(thread) ?? ""),
  );
  const currentThreadDecisions = getCurrentThreadDecisions(
    threadsRequiringDecision,
    codexReview.existingThreadDecisions,
  );

  const parsedDiff = parsePullRequestDiff(pullRequestDiff);
  for (const finding of codexReview.newFindings) {
    if (!isFindingOnChangedLines(finding, parsedDiff)) {
      throw new Error(
        `Finding "${finding.title}" is not anchored to changed lines in ${finding.path}.`,
      );
    }
  }

  const existingFingerprints = findExistingFingerprints(
    reviewContext.reviewThreads,
  );
  const newFindings = codexReview.newFindings.filter((finding) => {
    const fingerprint = fingerprintFinding(
      finding,
      reviewContext.pullRequest.headSha,
    );
    if (existingFingerprints.has(fingerprint)) return false;
    existingFingerprints.add(fingerprint);
    return true;
  });

  return {
    reviewedHeadSha: reviewContext.pullRequest.headSha,
    summary: codexReview.summary,
    newFindingCount: proposedFingerprints.size,
    newFindings,
    threadIdsToKeep: currentThreadDecisions
      .filter(({ disposition }) => disposition === "keep")
      .map(({ threadId }) => threadId),
    threadsToResolve: currentThreadDecisions
      .filter(({ disposition }) => disposition === "resolve")
      .map(({ threadId, lastCommentId }) => ({ threadId, lastCommentId })),
  };
}

export function fingerprintFinding(
  finding: NewFinding,
  reviewedHeadSha: string,
): string {
  const identity = JSON.stringify({
    reviewedHeadSha,
    path: finding.path,
    startLine: finding.startLine,
    endLine: finding.endLine,
    title: finding.title.trim().toLocaleLowerCase("en-US"),
  });
  return crypto.createHash("sha256").update(identity).digest("hex");
}

export function formatFindingComment(
  finding: NewFinding,
  reviewedHeadSha: string,
): string {
  const fingerprint = fingerprintFinding(finding, reviewedHeadSha);
  return `${REVIEW_MARKER}
<!-- wasp-code-review:fingerprint=${fingerprint} -->
**${finding.title}**

${finding.body}`;
}

function getCurrentThreadDecisions(
  reviewThreads: ReviewThread[],
  threadDecisions: ExistingThreadDecision[],
): ExistingThreadDecision[] {
  const threadsById = new Map(
    reviewThreads.map((thread) => [thread.id, thread]),
  );
  const currentDecisions = threadDecisions.filter((decision) => {
    const thread = threadsById.get(decision.threadId);
    if (!thread) {
      throw new Error(
        `Codex returned an unknown review thread: ${decision.threadId}.`,
      );
    }
    return !thread.isResolved;
  });
  const unresolvedThreads = reviewThreads.filter(
    (thread) => !thread.isResolved,
  );
  const decisionsByThreadId = new Map(
    currentDecisions.map((decision) => [decision.threadId, decision]),
  );

  if (
    decisionsByThreadId.size !== currentDecisions.length ||
    decisionsByThreadId.size !== unresolvedThreads.length
  ) {
    throw new Error(
      "Codex must return exactly one decision for every unresolved review thread.",
    );
  }

  for (const thread of unresolvedThreads) {
    const decision = decisionsByThreadId.get(thread.id);
    const lastComment = thread.comments.at(-1);
    if (!decision || decision.lastCommentId !== lastComment?.id) {
      throw new Error(
        `Review thread ${thread.id} changed while the review was running.`,
      );
    }
    if (decision.disposition === "resolve" && !thread.viewerCanResolve) {
      throw new Error(
        `Review thread ${thread.id} cannot be resolved by this token.`,
      );
    }
  }

  return currentDecisions;
}

function findExistingFingerprints(reviewThreads: ReviewThread[]): Set<string> {
  const fingerprints = new Set<string>();

  for (const thread of reviewThreads) {
    const fingerprint = findThreadFingerprint(thread);
    if (fingerprint) fingerprints.add(fingerprint);
  }

  return fingerprints;
}

function findThreadFingerprint(thread: ReviewThread): string | null {
  return thread.comments[0]?.body.match(FINGERPRINT_PATTERN)?.[1] ?? null;
}
