import * as z from "zod";

export const RepositorySchema = z.strictObject({
  owner: z.string().trim().min(1),
  name: z.string().trim().min(1),
});

export const GitShaSchema = z.string().regex(/^[0-9a-f]{40}$/i);

export const ReviewThreadCommentSchema = z.strictObject({
  id: z.string().trim().min(1),
  author: z.strictObject({ login: z.string().trim().min(1) }).nullable(),
  body: z.string(),
});

export const ReviewThreadSchema = z.strictObject({
  id: z.string().trim().min(1),
  isResolved: z.boolean(),
  isOutdated: z.boolean(),
  path: z.string().trim().min(1),
  line: z.int().positive().nullable(),
  startLine: z.int().positive().nullable(),
  viewerCanResolve: z.boolean(),
  comments: z.array(ReviewThreadCommentSchema).min(1),
});

export const ReviewContextSchema = z.strictObject({
  reviewedHeadSha: GitShaSchema,
  reviewerLogin: z.string().trim().min(1),
  reviewThreads: z.array(ReviewThreadSchema),
});

export const NewFindingSchema = z.strictObject({
  body: z
    .string()
    .trim()
    .min(1)
    .max(3_000)
    .describe(
      "A concise, natural paragraph explaining the trigger, incorrect behavior, consequence, and correction.",
    ),
  suggestion: z
    .string()
    .min(1)
    .max(5_000)
    .regex(/\S/, "Suggestion must contain code.")
    .nullable()
    .describe(
      "Complete replacement with exact indentation for the selected lines, without Markdown fences; null when no safe replacement is available.",
    ),
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

export const ThreadResolutionSchema = z.strictObject({
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
});

export const CodexOutputSchema = z.strictObject({
  threadsToResolve: z
    .array(ThreadResolutionSchema)
    .describe("Addressed reviewer threads to resolve; omission means keep."),
  newFindings: z
    .array(NewFindingSchema)
    .max(5)
    .describe("Up to five meaningful issues not covered by existing threads."),
});

export type Repository = z.infer<typeof RepositorySchema>;
export type ReviewThread = z.infer<typeof ReviewThreadSchema>;
export type ReviewContext = z.infer<typeof ReviewContextSchema>;
export type NewFinding = z.infer<typeof NewFindingSchema>;
export type ThreadResolution = z.infer<typeof ThreadResolutionSchema>;
export type CodexReview = z.infer<typeof CodexOutputSchema>;
export type PublicationPlan = {
  reviewedHeadSha: string;
  newFindings: NewFinding[];
  threadsToResolve: { threadId: string; lastCommentId: string }[];
};

export function parseRepositorySlug(repositorySlug: string): Repository {
  const [owner, name, ...unexpectedParts] = repositorySlug.split("/");
  if (!owner || !name || unexpectedParts.length > 0) {
    throw new Error(`Invalid GitHub repository: ${repositorySlug}`);
  }

  return RepositorySchema.parse({ owner, name });
}
