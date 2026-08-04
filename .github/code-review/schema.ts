import path from "node:path";
import * as z from "zod";

export const RepositorySchema = z.strictObject({
  owner: z.string().trim().min(1),
  name: z.string().trim().min(1),
});

export const GitShaSchema = z.string().regex(/^[0-9a-f]{40}$/i);

export const PullRequestSchema = z.strictObject({
  number: z.int().positive(),
  baseSha: GitShaSchema,
  headSha: GitShaSchema,
  state: z.enum(["OPEN", "CLOSED", "MERGED"]),
  isDraft: z.boolean(),
});

export const ReviewThreadCommentSchema = z.strictObject({
  id: z.string().trim().min(1),
  authorLogin: z.string().trim().min(1).nullable(),
  body: z.string(),
});

export const ReviewThreadSchema = z.strictObject({
  id: z.string().trim().min(1),
  isResolved: z.boolean(),
  isOutdated: z.boolean(),
  path: z.string().trim().min(1),
  line: z.int().positive().nullable(),
  startLine: z.int().positive().nullable(),
  canResolve: z.boolean(),
  comments: z.array(ReviewThreadCommentSchema).min(1),
});

export const ReviewContextSchema = z.strictObject({
  repository: RepositorySchema,
  pullRequest: PullRequestSchema,
  reviewerLogin: z.string().trim().min(1),
  reviewThreads: z.array(ReviewThreadSchema),
});

export const NewFindingSchema = z.strictObject({
  title: z.string().trim().min(1).max(80).describe("Concise issue title."),
  body: z
    .string()
    .trim()
    .min(1)
    .max(5_000)
    .describe("Problem, impact, and suggested fix in at most three sentences."),
  path: z
    .string()
    .trim()
    .min(1)
    .max(1_024)
    .describe("Normalized repository-relative POSIX path."),
  startLine: z
    .int()
    .min(1)
    .describe("First changed line covered by the issue."),
  endLine: z.int().min(1).describe("Last changed line covered by the issue."),
});

export const ExistingThreadDecisionSchema = z.strictObject({
  threadId: z
    .string()
    .trim()
    .min(1)
    .describe("ID of an unresolved thread supplied in the review context."),
  lastCommentId: z
    .string()
    .trim()
    .min(1)
    .describe("ID of the thread's last comment when it was reviewed."),
  disposition: z
    .enum(["keep", "resolve"])
    .describe(
      "Keep an open concern untouched, or resolve an addressed concern.",
    ),
});

export const CodexOutputSchema = z.strictObject({
  summary: z
    .string()
    .trim()
    .min(1)
    .max(10_000)
    .describe("One-sentence summary of the current review result."),
  existingThreadDecisions: z
    .array(ExistingThreadDecisionSchema)
    .describe("Exactly one decision for every unresolved reviewer thread."),
  newFindings: z
    .array(NewFindingSchema)
    .max(5)
    .describe("Up to five meaningful issues not covered by existing threads."),
});

export const ReviewSchema = CodexOutputSchema.superRefine(
  ({ existingThreadDecisions, newFindings }, context) => {
    const seenThreadIds = new Set<string>();
    existingThreadDecisions.forEach(({ threadId }, index) => {
      if (seenThreadIds.has(threadId)) {
        context.addIssue({
          code: "custom",
          path: ["existingThreadDecisions", index, "threadId"],
          message: "Each existing thread must have exactly one decision.",
        });
      }
      seenThreadIds.add(threadId);
    });

    newFindings.forEach((finding, index) => {
      if (!isNormalizedRelativePosixPath(finding.path)) {
        context.addIssue({
          code: "custom",
          path: ["newFindings", index, "path"],
          message: "Path must be a normalized, relative POSIX path.",
        });
      }

      if (finding.endLine < finding.startLine) {
        context.addIssue({
          code: "custom",
          path: ["newFindings", index, "endLine"],
          message: "End line must not precede start line.",
        });
      }
    });
  },
);

export type Repository = z.infer<typeof RepositorySchema>;
export type PullRequest = z.infer<typeof PullRequestSchema>;
export type ReviewThread = z.infer<typeof ReviewThreadSchema>;
export type ReviewContext = z.infer<typeof ReviewContextSchema>;
export type NewFinding = z.infer<typeof NewFindingSchema>;
export type ExistingThreadDecision = z.infer<
  typeof ExistingThreadDecisionSchema
>;
export type CodexReview = z.infer<typeof ReviewSchema>;
export type InlineReviewComment = Pick<
  NewFinding,
  "body" | "endLine" | "path" | "startLine"
>;
export type PublicationPlan = {
  reviewedHeadSha: string;
  summary: string;
  newFindings: NewFinding[];
  threadIdsToKeep: string[];
  threadsToResolve: { threadId: string; lastCommentId: string }[];
};

export function parseRepositorySlug(repositorySlug: string): Repository {
  const [owner, name, ...unexpectedParts] = repositorySlug.split("/");
  if (!owner || !name || unexpectedParts.length > 0) {
    throw new Error(`Invalid GitHub repository: ${repositorySlug}`);
  }

  return RepositorySchema.parse({ owner, name });
}

function isNormalizedRelativePosixPath(filePath: string): boolean {
  const normalizedPath = path.posix.normalize(filePath);
  return (
    normalizedPath === filePath &&
    normalizedPath !== "." &&
    !normalizedPath.startsWith("../") &&
    !path.posix.isAbsolute(normalizedPath) &&
    !path.win32.isAbsolute(filePath) &&
    !filePath.includes("\\")
  );
}
