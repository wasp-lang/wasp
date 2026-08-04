import fs from "node:fs";
import * as z from "zod";
import { REVIEW_CONTEXT_FILE } from "../config.ts";
import { createGitHubClient } from "../github-api.ts";
import { loadReviewContext } from "../review-context.ts";
import { GitShaSchema, parseRepositorySlug } from "../schema.ts";

const EnvironmentSchema = z.object({
  EXPECTED_HEAD_SHA: GitShaSchema,
  GH_TOKEN: z.string().min(1),
  PR_NUMBER: z.coerce.number().int().positive(),
  REPOSITORY: z.string().min(1),
});

const environment = EnvironmentSchema.parse(process.env);
const repository = parseRepositorySlug(environment.REPOSITORY);
const github = createGitHubClient({ token: environment.GH_TOKEN });
const reviewContext = await loadReviewContext({
  github,
  repository,
  pullNumber: environment.PR_NUMBER,
  expectedHeadSha: environment.EXPECTED_HEAD_SHA,
});
const repositoryRoot = new URL("../../../", import.meta.url);

fs.writeFileSync(
  new URL(REVIEW_CONTEXT_FILE, repositoryRoot),
  `${JSON.stringify(reviewContext, null, 2)}\n`,
);
