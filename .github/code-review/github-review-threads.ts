import type { GitHubOctokit } from "./github.ts";
import type { Repository, ReviewThread } from "./schema.ts";

type PageInfo = {
  endCursor: string | null;
  hasNextPage: boolean;
};

type ReviewThreadResponse = Omit<ReviewThread, "comments"> & {
  comments: {
    nodes: (ReviewThread["comments"][number] | null)[];
    totalCount: number;
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

type ThreadCommentsResponse = {
  node: {
    comments: {
      nodes: (ReviewThread["comments"][number] | null)[];
      pageInfo: PageInfo;
    };
  } | null;
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

  const reviewThreads = await Promise.all(
    compact(pullRequest.reviewThreads.nodes).map((thread) =>
      completeReviewThread(octokit, thread),
    ),
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
  return completeReviewThread(octokit, response.node);
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

async function completeReviewThread(
  octokit: GitHubOctokit,
  thread: ReviewThreadResponse,
): Promise<ReviewThread> {
  const comments =
    thread.comments.totalCount > thread.comments.nodes.length
      ? await fetchAllThreadComments(octokit, thread.id)
      : compact(thread.comments.nodes);

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

async function fetchAllThreadComments(
  octokit: GitHubOctokit,
  threadId: string,
): Promise<ReviewThread["comments"]> {
  const response = await octokit.graphql.paginate<ThreadCommentsResponse>(
    THREAD_COMMENTS_QUERY,
    { threadId },
  );
  if (!response.node) {
    throw new Error(`Review thread ${threadId} was not found.`);
  }
  return compact(response.node.comments.nodes);
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
  comments(first: 100) {
    nodes { id author { login } body }
    totalCount
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

const THREAD_COMMENTS_QUERY = `
  query ThreadComments($threadId: ID!, $cursor: String) {
    node(id: $threadId) {
      ... on PullRequestReviewThread {
        comments(first: 100, after: $cursor) {
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
