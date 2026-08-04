import { paginateGraphQL } from "@octokit/plugin-paginate-graphql";
import { Octokit } from "@octokit/rest";

export const GitHubOctokit = Octokit.plugin(paginateGraphQL);
export type GitHubOctokit = InstanceType<typeof GitHubOctokit>;
