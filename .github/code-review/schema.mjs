import path from "node:path";
import * as z from "zod";

const FindingSchema = z.strictObject({
  title: z.string().trim().min(1).max(80),
  body: z.string().trim().min(1).max(5_000),
  severity: z.enum(["ERROR", "WARNING", "INFO"]),
  path: z.string().trim().min(1).max(1_024),
  startLine: z.int().min(1),
  endLine: z.int().min(1),
});

export const CodexOutputSchema = z.strictObject({
  summary: z.string().trim().min(1).max(10_000),
  findings: z.array(FindingSchema).max(10),
});

export const ReviewSchema = CodexOutputSchema.superRefine(validateReview);

function validateReview({ findings }, context) {
  findings.forEach((finding, index) => {
    validateFindingPath(finding, index, context);
    validateFindingRange(finding, index, context);
  });
}

function validateFindingPath(finding, index, context) {
  if (!isNormalizedRelativePosixPath(finding.path)) {
    context.addIssue({
      code: "custom",
      path: ["findings", index, "path"],
      message: "Path must be a normalized, relative POSIX path.",
    });
  }
}

function validateFindingRange(finding, index, context) {
  if (finding.endLine < finding.startLine) {
    context.addIssue({
      code: "custom",
      path: ["findings", index, "endLine"],
      message: "End line must not precede start line.",
    });
  }
}

function isNormalizedRelativePosixPath(filePath) {
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
