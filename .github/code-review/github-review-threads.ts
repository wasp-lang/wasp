import { MAX_RECENT_THREAD_COMMENTS } from "./config.ts";
import type { GitHubOctokit } from "./github.ts";
import type { Repository, ReviewThread } from "./schema.ts";

type PageInfo = {
  endCursor: string | null;
  hasNextPage: boolean;
};

type ReviewThreadResponse = Omit<ReviewThread, "comments"> & {
  comments: {
    nodes: (ReviewThread["comments"][number] | null)[];
  };
  recentComments: {
    nodes: (ReviewThread["comments"][number] | null)[];
  };
};

type ReviewThreadsResponse = {
  viewer: { login: string };
  repository: {
    pullRequest: {
      reviewThreads: {
        nodes: (ReviewThreadResponse | null)[];
        pageInfo: PageInfo;
      };
    } | null;
  } | null;
};

type ReviewThreadResponseData = {
  node: ReviewThreadResponse | null;
};

export type ReviewSnapshot = {
  reviewerLogin: string;
  reviewThreads: ReviewThread[];
};

export async function fetchReviewSnapshot(
  octokit: GitHubOctokit,
  repository: Repository,
  pullNumber: number,
): Promise<ReviewSnapshot> {
  const response = await octokit.graphql.paginate<ReviewThreadsResponse>(
    REVIEW_THREADS_QUERY,
    {
      owner: repository.owner,
      name: repository.name,
      number: pullNumber,
    },
  );
  const pullRequest = response.repository?.pullRequest;
  if (!pullRequest) {
    throw new Error(`Pull request #${pullNumber} was not found.`);
  }

  const reviewThreads = compact(pullRequest.reviewThreads.nodes).map(
    normalizeReviewThread,
  );
  return { reviewerLogin: response.viewer.login, reviewThreads };
}

export async function fetchReviewThread(
  octokit: GitHubOctokit,
  threadId: string,
): Promise<ReviewThread> {
  const response = await octokit.graphql<ReviewThreadResponseData>(
    REVIEW_THREAD_QUERY,
    { threadId },
  );
  if (!response.node) {
    throw new Error(`Review thread ${threadId} was not found.`);
  }
  return normalizeReviewThread(response.node);
}

export async function resolveReviewThread(
  octokit: GitHubOctokit,
  threadId: string,
): Promise<void> {
  const response = await octokit.graphql<{
    resolveReviewThread: { thread: { id: string; isResolved: boolean } };
  }>(RESOLVE_REVIEW_THREAD_MUTATION, { input: { threadId } });

  if (
    response.resolveReviewThread.thread.id !== threadId ||
    !response.resolveReviewThread.thread.isResolved
  ) {
    throw new Error("GitHub resolved an unexpected review thread.");
  }
}

function normalizeReviewThread(thread: ReviewThreadResponse): ReviewThread {
  const firstComments = compact(thread.comments.nodes);
  const recentComments = thread.isResolved
    ? []
    : compact(thread.recentComments.nodes);
  const comments = [...firstComments, ...recentComments].filter(
    (comment, index, allComments) =>
      allComments.findIndex(({ id }) => id === comment.id) === index,
  );

  return {
    id: thread.id,
    isResolved: thread.isResolved,
    isOutdated: thread.isOutdated,
    path: thread.path,
    line: thread.line,
    startLine: thread.startLine,
    viewerCanResolve: thread.viewerCanResolve,
    comments,
  };
}

function compact<Value>(values: (Value | null)[]): Value[] {
  return values.filter((value): value is Value => value !== null);
}

const REVIEW_THREAD_FIELDS = `
  id
  isResolved
  isOutdated
  path
  line
  startLine
  viewerCanResolve
  comments(first: 1) {
    nodes { id author { login } body }
  }
  recentComments: comments(last: ${MAX_RECENT_THREAD_COMMENTS}) {
    nodes { id author { login } body }
  }
`;

const REVIEW_THREADS_QUERY = `
  query ReviewThreads(
    $owner: String!
    $name: String!
    $number: Int!
    $cursor: String
  ) {
    viewer { login }
    repository(owner: $owner, name: $name) {
      pullRequest(number: $number) {
        reviewThreads(first: 100, after: $cursor) {
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

const RESOLVE_REVIEW_THREAD_MUTATION = `
  mutation ResolveReviewThread($input: ResolveReviewThreadInput!) {
    resolveReviewThread(input: $input) {
      thread { id isResolved }
    }
  }
`;
