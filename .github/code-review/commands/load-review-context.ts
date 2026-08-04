import fs from "node:fs";
import * as z from "zod";
import { REVIEW_CONTEXT_FILE } from "../config.ts";
import { GitHubOctokit } from "../github.ts";
import { loadReviewContext } from "../review-context.ts";
import { GitShaSchema, parseRepositorySlug } from "../schema.ts";

const EnvironmentSchema = z.object({
  EXPECTED_BASE_SHA: GitShaSchema,
  EXPECTED_HEAD_SHA: GitShaSchema,
  GH_TOKEN: z.string().min(1),
  PR_NUMBER: z.coerce.number().int().positive(),
  REPOSITORY: z.string().min(1),
});

const environment = EnvironmentSchema.parse(process.env);
const repository = parseRepositorySlug(environment.REPOSITORY);
const octokit = new GitHubOctokit({ auth: environment.GH_TOKEN });
const reviewContext = await loadReviewContext({
  octokit,
  repository,
  pullNumber: environment.PR_NUMBER,
  expectedBaseSha: environment.EXPECTED_BASE_SHA,
  expectedHeadSha: environment.EXPECTED_HEAD_SHA,
});
const repositoryRoot = new URL("../../../", import.meta.url);

fs.writeFileSync(
  new URL(REVIEW_CONTEXT_FILE, repositoryRoot),
  `${JSON.stringify(reviewContext, null, 2)}\n`,
);
