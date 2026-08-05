import { execFileSync } from "node:child_process";
import * as z from "zod";
import { GitHubOctokit } from "../github.ts";
import { publishCodeReview } from "../review-publisher.ts";
import {
  CodexOutputSchema,
  GitShaSchema,
  parseRepositorySlug,
} from "../schema.ts";

const EnvironmentSchema = z.object({
  CODEX_MODEL: z.string().min(1),
  CODEX_REVIEW_JSON: z.string().min(1),
  EXPECTED_BASE_SHA: GitShaSchema,
  EXPECTED_HEAD_SHA: GitShaSchema,
  GH_TOKEN: z.string().min(1),
  PR_NUMBER: z.coerce.number().int().positive(),
  REPOSITORY: z.string().min(1),
});

const environment = EnvironmentSchema.parse(process.env);
const octokit = new GitHubOctokit({ auth: environment.GH_TOKEN });
const pullRequestDiff = execFileSync(
  "git",
  [
    "-c",
    "core.quotePath=false",
    "diff",
    "--no-ext-diff",
    `${environment.EXPECTED_BASE_SHA}...${environment.EXPECTED_HEAD_SHA}`,
  ],
  { encoding: "utf8", maxBuffer: 100 * 1024 * 1024 },
);

await publishCodeReview({
  octokit,
  repository: parseRepositorySlug(environment.REPOSITORY),
  pullNumber: environment.PR_NUMBER,
  expectedBaseSha: environment.EXPECTED_BASE_SHA,
  expectedHeadSha: environment.EXPECTED_HEAD_SHA,
  pullRequestDiff,
  modelUsed: environment.CODEX_MODEL,
  codexReview: CodexOutputSchema.parse(
    JSON.parse(environment.CODEX_REVIEW_JSON) as unknown,
  ),
});
