import crypto from "node:crypto";
import { REVIEW_MARKER } from "./config.ts";
import {
  isFindingOnChangedLines,
  parsePullRequestDiff,
} from "./pull-request-diff.ts";
import {
  type CodexReview,
  type NewFinding,
  type PublicationPlan,
  type ReviewContext,
  type ReviewThread,
  type ThreadResolution,
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
  const currentThreadResolutions = getCurrentThreadResolutions(
    reviewContext.reviewThreads,
    codexReview.threadsToResolve,
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
    newFindings,
    threadsToResolve: currentThreadResolutions,
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
  const suggestion = finding.suggestion
    ? `\n\n\`\`\`suggestion\n${finding.suggestion}\n\`\`\``
    : "";
  return `${REVIEW_MARKER}
<!-- wasp-code-review:fingerprint=${fingerprint} -->
${finding.body}${suggestion}`;
}

function getCurrentThreadResolutions(
  reviewThreads: ReviewThread[],
  threadResolutions: ThreadResolution[],
): ThreadResolution[] {
  const threadsById = new Map(
    reviewThreads.map((thread) => [thread.id, thread]),
  );
  return threadResolutions.filter((resolution) => {
    const thread = threadsById.get(resolution.threadId);
    if (!thread) {
      throw new Error(
        `Codex returned an unknown review thread: ${resolution.threadId}.`,
      );
    }
    if (thread.isResolved) return false;
    const lastComment = thread.comments.at(-1);
    if (resolution.lastCommentId !== lastComment?.id) {
      throw new Error(
        `Review thread ${thread.id} changed while the review was running.`,
      );
    }
    if (!thread.viewerCanResolve) {
      throw new Error(
        `Review thread ${thread.id} cannot be resolved by this token.`,
      );
    }
    return true;
  });
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
