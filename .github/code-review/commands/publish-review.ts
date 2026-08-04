import * as z from "zod";
import { createGitHubClient } from "../github-api.ts";
import { publishCodeReview } from "../review-publisher.ts";
import { GitShaSchema, ReviewSchema, parseRepositorySlug } from "../schema.ts";

const EnvironmentSchema = z.object({
  CODEX_REVIEW_JSON: z.string().min(1),
  EXPECTED_HEAD_SHA: GitShaSchema,
  GH_TOKEN: z.string().min(1),
  PR_NUMBER: z.coerce.number().int().positive(),
  REPOSITORY: z.string().min(1),
});

const environment = EnvironmentSchema.parse(process.env);
const github = createGitHubClient({ token: environment.GH_TOKEN });

await publishCodeReview({
  github,
  repository: parseRepositorySlug(environment.REPOSITORY),
  pullNumber: environment.PR_NUMBER,
  expectedHeadSha: environment.EXPECTED_HEAD_SHA,
  codexReview: ReviewSchema.parse(
    JSON.parse(environment.CODEX_REVIEW_JSON) as unknown,
  ),
});
