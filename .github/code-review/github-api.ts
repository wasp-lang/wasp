import * as z from "zod";
import {
  PullRequestSchema,
  type InlineReviewComment,
  type PullRequest,
  type Repository,
  type ReviewThread,
} from "./schema.ts";

const GITHUB_API_URL = "https://api.github.com";
const GITHUB_GRAPHQL_URL = `${GITHUB_API_URL}/graphql`;

const PullRequestResponseSchema = z.object({
  number: z.int(),
  state: z.enum(["open", "closed"]),
  draft: z.boolean(),
  merged: z.boolean(),
  base: z.object({ sha: z.string() }),
  head: z.object({ sha: z.string() }),
});

const ReviewCommentResponseSchema = z.object({
  id: z.string(),
  body: z.string(),
  author: z.object({ login: z.string() }).nullable(),
});

const PageInfoSchema = z.object({
  endCursor: z.string().nullable(),
  hasNextPage: z.boolean(),
});

const ReviewCommentsResponseSchema = z.object({
  nodes: z.array(ReviewCommentResponseSchema.nullable()),
  pageInfo: PageInfoSchema,
});

const ReviewThreadResponseSchema = z.object({
  id: z.string(),
  isResolved: z.boolean(),
  isOutdated: z.boolean(),
  path: z.string(),
  line: z.int().nullable(),
  startLine: z.int().nullable(),
  viewerCanResolve: z.boolean(),
  comments: ReviewCommentsResponseSchema,
});

const ReviewThreadsPageSchema = z.object({
  viewer: z.object({ login: z.string() }),
  repository: z
    .object({
      pullRequest: z
        .object({
          reviewThreads: z.object({
            nodes: z.array(ReviewThreadResponseSchema.nullable()),
            pageInfo: PageInfoSchema,
          }),
        })
        .nullable(),
    })
    .nullable(),
});

const ReviewThreadPageSchema = z.object({
  node: ReviewThreadResponseSchema.nullable(),
});

const ThreadCommentsPageSchema = z.object({
  node: z.object({ comments: ReviewCommentsResponseSchema }).nullable(),
});

const GraphqlResponseSchema = z.object({
  data: z.unknown().optional(),
  errors: z.array(z.object({ message: z.string() })).optional(),
});

const IssueCommentSchema = z.object({
  id: z.int(),
  body: z.string().nullable(),
  user: z.object({ login: z.string() }).nullable(),
});

type RequestOptions = {
  method?: "GET" | "POST" | "PATCH";
  accept?: string;
  body?: unknown;
};

export type ReviewSnapshot = {
  reviewerLogin: string;
  reviewThreads: ReviewThread[];
};

export function createGitHubClient({
  token,
  fetch: fetchImplementation = globalThis.fetch,
}: {
  token: string;
  fetch?: typeof globalThis.fetch;
}) {
  async function request(
    path: string,
    options: RequestOptions = {},
  ): Promise<Response> {
    const response = await fetchImplementation(
      path.startsWith("https://") ? path : `${GITHUB_API_URL}${path}`,
      {
        method: options.method ?? "GET",
        headers: {
          Accept: options.accept ?? "application/vnd.github+json",
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
          "X-GitHub-Api-Version": "2022-11-28",
        },
        ...(options.body === undefined
          ? {}
          : { body: JSON.stringify(options.body) }),
      },
    );

    if (!response.ok) {
      throw new Error(
        `GitHub API request failed (${response.status} ${response.statusText}): ${await response.text()}`,
      );
    }
    return response;
  }

  async function requestJson<Schema extends z.ZodType>(
    path: string,
    schema: Schema,
    options?: RequestOptions,
  ): Promise<z.output<Schema>> {
    const response = await request(path, options);
    return schema.parse(await response.json());
  }

  async function requestGraphql(
    query: string,
    variables: Record<string, unknown>,
  ): Promise<unknown> {
    const response = await request(GITHUB_GRAPHQL_URL, {
      method: "POST",
      body: { query, variables },
    });
    const envelope = GraphqlResponseSchema.parse(await response.json());

    if (envelope.errors?.length) {
      throw new Error(
        `GitHub GraphQL request failed: ${envelope.errors.map(({ message }) => message).join("; ")}`,
      );
    }
    if (envelope.data === undefined) {
      throw new Error("GitHub GraphQL response did not contain data.");
    }
    return envelope.data;
  }

  async function fetchRemainingComments(
    threadId: string,
    initialCursor: string | null,
  ): Promise<ReviewThread["comments"]> {
    const comments: ReviewThread["comments"] = [];
    let after = initialCursor;

    while (after) {
      const page = ThreadCommentsPageSchema.parse(
        await requestGraphql(THREAD_COMMENTS_QUERY, { threadId, after }),
      );
      if (!page.node) {
        throw new Error(`Review thread ${threadId} was not found.`);
      }
      comments.push(...compact(page.node.comments.nodes).map(mapReviewComment));
      after = nextCursor(page.node.comments.pageInfo);
    }
    return comments;
  }

  async function completeReviewThread(
    thread: z.infer<typeof ReviewThreadResponseSchema>,
  ): Promise<ReviewThread> {
    const comments = compact(thread.comments.nodes).map(mapReviewComment);
    if (thread.comments.pageInfo.hasNextPage) {
      comments.push(
        ...(await fetchRemainingComments(
          thread.id,
          thread.comments.pageInfo.endCursor,
        )),
      );
    }

    return {
      id: thread.id,
      isResolved: thread.isResolved,
      isOutdated: thread.isOutdated,
      path: thread.path,
      line: thread.line,
      startLine: thread.startLine,
      canResolve: thread.viewerCanResolve,
      comments,
    };
  }

  async function fetchPullRequest(
    repository: Repository,
    pullNumber: number,
  ): Promise<PullRequest> {
    const response = await requestJson(
      `/repos/${repositoryPath(repository)}/pulls/${pullNumber}`,
      PullRequestResponseSchema,
    );
    return PullRequestSchema.parse({
      number: response.number,
      baseSha: response.base.sha,
      headSha: response.head.sha,
      state: response.merged ? "MERGED" : response.state.toUpperCase(),
      isDraft: response.draft,
    });
  }

  async function fetchPullRequestDiff(
    repository: Repository,
    pullNumber: number,
  ): Promise<string> {
    const response = await request(
      `/repos/${repositoryPath(repository)}/pulls/${pullNumber}`,
      { accept: "application/vnd.github.diff" },
    );
    return response.text();
  }

  async function fetchReviewSnapshot(
    repository: Repository,
    pullNumber: number,
  ): Promise<ReviewSnapshot> {
    const reviewThreads: ReviewThread[] = [];
    let reviewerLogin: string | null = null;
    let after: string | null = null;

    do {
      const page = ReviewThreadsPageSchema.parse(
        await requestGraphql(REVIEW_THREADS_QUERY, {
          owner: repository.owner,
          name: repository.name,
          number: pullNumber,
          after,
        }),
      );
      const pullRequest = page.repository?.pullRequest;
      if (!pullRequest) {
        throw new Error(`Pull request #${pullNumber} was not found.`);
      }

      reviewerLogin ??= page.viewer.login;
      for (const thread of compact(pullRequest.reviewThreads.nodes)) {
        reviewThreads.push(await completeReviewThread(thread));
      }
      after = nextCursor(pullRequest.reviewThreads.pageInfo);
    } while (after);

    if (!reviewerLogin) {
      throw new Error("GitHub GraphQL response did not identify the reviewer.");
    }
    return { reviewerLogin, reviewThreads };
  }

  async function fetchReviewThread(threadId: string): Promise<ReviewThread> {
    const page = ReviewThreadPageSchema.parse(
      await requestGraphql(REVIEW_THREAD_QUERY, { threadId }),
    );
    if (!page.node) {
      throw new Error(`Review thread ${threadId} was not found.`);
    }
    return completeReviewThread(page.node);
  }

  async function submitPullRequestReview({
    repository,
    pullNumber,
    commitSha,
    body,
    comments,
  }: {
    repository: Repository;
    pullNumber: number;
    commitSha: string;
    body: string;
    comments: InlineReviewComment[];
  }): Promise<void> {
    await request(
      `/repos/${repositoryPath(repository)}/pulls/${pullNumber}/reviews`,
      {
        method: "POST",
        body: {
          commit_id: commitSha,
          event: "COMMENT",
          body,
          comments: comments.map(toGitHubReviewComment),
        },
      },
    );
  }

  async function resolveReviewThread(threadId: string): Promise<void> {
    const response = z
      .object({
        resolveReviewThread: z.object({
          thread: z.object({ id: z.string(), isResolved: z.literal(true) }),
        }),
      })
      .parse(
        await requestGraphql(RESOLVE_REVIEW_THREAD_MUTATION, {
          input: { threadId },
        }),
      );
    if (response.resolveReviewThread.thread.id !== threadId) {
      throw new Error("GitHub resolved an unexpected review thread.");
    }
  }

  async function fetchIssueComments(
    repository: Repository,
    pullNumber: number,
  ): Promise<z.infer<typeof IssueCommentSchema>[]> {
    const comments: z.infer<typeof IssueCommentSchema>[] = [];
    let path: string | null =
      `/repos/${repositoryPath(repository)}/issues/${pullNumber}/comments?per_page=100`;

    while (path) {
      const response = await request(path);
      comments.push(
        ...z.array(IssueCommentSchema).parse(await response.json()),
      );
      path = nextPagePath(response.headers.get("link"));
    }
    return comments;
  }

  async function createOrUpdateReviewSummary({
    repository,
    pullNumber,
    marker,
    reviewerLogin,
    body,
  }: {
    repository: Repository;
    pullNumber: number;
    marker: string;
    reviewerLogin: string;
    body: string;
  }): Promise<void> {
    const comments = await fetchIssueComments(repository, pullNumber);
    const existingSummary = comments.find(
      (comment) =>
        comment.user?.login === reviewerLogin && comment.body?.includes(marker),
    );

    if (existingSummary) {
      await request(
        `/repos/${repositoryPath(repository)}/issues/comments/${existingSummary.id}`,
        { method: "PATCH", body: { body } },
      );
    } else {
      await request(
        `/repos/${repositoryPath(repository)}/issues/${pullNumber}/comments`,
        { method: "POST", body: { body } },
      );
    }
  }

  return {
    createOrUpdateReviewSummary,
    fetchPullRequest,
    fetchPullRequestDiff,
    fetchReviewSnapshot,
    fetchReviewThread,
    resolveReviewThread,
    submitPullRequestReview,
  };
}

export type GitHubClient = ReturnType<typeof createGitHubClient>;

function repositoryPath({ owner, name }: Repository): string {
  return `${encodeURIComponent(owner)}/${encodeURIComponent(name)}`;
}

function compact<Value>(values: (Value | null)[]): Value[] {
  return values.filter((value): value is Value => value !== null);
}

function mapReviewComment(
  comment: z.infer<typeof ReviewCommentResponseSchema>,
): ReviewThread["comments"][number] {
  return {
    id: comment.id,
    authorLogin: comment.author?.login ?? null,
    body: comment.body,
  };
}

function toGitHubReviewComment(comment: InlineReviewComment): object {
  return {
    path: comment.path,
    body: comment.body,
    line: comment.endLine,
    side: "RIGHT",
    ...(comment.startLine === comment.endLine
      ? {}
      : { start_line: comment.startLine, start_side: "RIGHT" }),
  };
}

function nextPagePath(linkHeader: string | null): string | null {
  if (!linkHeader) return null;

  for (const link of linkHeader.split(",")) {
    const match = link.match(/<([^>]+)>;\s*rel="([^"]+)"/);
    if (match?.[2] === "next") {
      const url = new URL(match[1]);
      return `${url.pathname}${url.search}`;
    }
  }
  return null;
}

function nextCursor(pageInfo: z.infer<typeof PageInfoSchema>): string | null {
  if (!pageInfo.hasNextPage) return null;
  if (!pageInfo.endCursor) {
    throw new Error(
      "GitHub pagination response did not include an end cursor.",
    );
  }
  return pageInfo.endCursor;
}

const REVIEW_THREAD_FIELDS = `
  id
  isResolved
  isOutdated
  path
  line
  startLine
  viewerCanResolve
  comments(first: 100) {
    nodes { id author { login } body }
    pageInfo { endCursor hasNextPage }
  }
`;

const REVIEW_THREADS_QUERY = `
  query ReviewThreads(
    $owner: String!
    $name: String!
    $number: Int!
    $after: String
  ) {
    viewer { login }
    repository(owner: $owner, name: $name) {
      pullRequest(number: $number) {
        reviewThreads(first: 100, after: $after) {
          nodes { ${REVIEW_THREAD_FIELDS} }
          pageInfo { endCursor hasNextPage }
        }
      }
    }
  }
`;

const REVIEW_THREAD_QUERY = `
  query ReviewThread($threadId: ID!) {
    node(id: $threadId) {
      ... on PullRequestReviewThread { ${REVIEW_THREAD_FIELDS} }
    }
  }
`;

const THREAD_COMMENTS_QUERY = `
  query ThreadComments($threadId: ID!, $after: String) {
    node(id: $threadId) {
      ... on PullRequestReviewThread {
        comments(first: 100, after: $after) {
          nodes { id author { login } body }
          pageInfo { endCursor hasNextPage }
        }
      }
    }
  }
`;

const RESOLVE_REVIEW_THREAD_MUTATION = `
  mutation ResolveReviewThread($input: ResolveReviewThreadInput!) {
    resolveReviewThread(input: $input) {
      thread { id isResolved }
    }
  }
`;
