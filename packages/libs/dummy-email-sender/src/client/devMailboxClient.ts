import type {
  ActionResponse,
  ApiErrorResponse,
  DeleteEmailRequest,
  EmailResponse,
  ListEmailsResponse,
  WaitForEmailRequest,
} from "../api/apiContract.js";
import { ApiError, type ApiErrorCode } from "../api/apiError.js";
import { devMailboxApiPaths } from "../api/apiPaths.js";
import type { DevMailboxEmail, DevMailboxEmailSummary } from "../core/email.js";
import type { DevMailboxEmailFilter } from "../core/emailFilter.js";

export type DevMailboxClientWaitForOptions = {
  /** Defaults to 10 seconds, at most 5 minutes. */
  timeoutMs?: number;
  signal?: AbortSignal;
};

/** The dev mailbox over HTTP, mirroring `DevMailbox`. */
export interface DevMailboxClient {
  /** Newest first. */
  list(
    filter?: DevMailboxEmailFilter,
    limit?: number,
  ): Promise<DevMailboxEmailSummary[]>;
  get(id: string): Promise<DevMailboxEmail | undefined>;
  /**
   * Resolves with the newest match, waiting for one if there is none yet.
   * Resolves with `undefined` on timeout.
   */
  waitFor(
    filter: DevMailboxEmailFilter,
    options?: DevMailboxClientWaitForOptions,
  ): Promise<DevMailboxEmail | undefined>;
  /** Returns whether the email existed. */
  delete(id: string): Promise<boolean>;
  clear(): Promise<void>;
}

/** Other error responses reject with an `ApiError`. */
export function createDevMailboxApiClient(apiUrl: string): DevMailboxClient {
  async function request<Response>(
    path: string,
    init?: RequestInit,
  ): Promise<Response> {
    const response = await fetch(apiUrl + path, init);
    if (!response.ok) {
      throw await toApiError(response);
    }
    return (await response.json()) as Response;
  }

  function post<Response>(
    path: string,
    body: unknown,
    signal?: AbortSignal,
  ): Promise<Response> {
    return request(path, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
      signal,
    });
  }

  return {
    async list(filter = {}, limit) {
      const { emails } = await request<ListEmailsResponse>(
        devMailboxApiPaths.emails + toQueryString(filter, limit),
      );
      return emails;
    },

    async get(id) {
      const response = await resolveUndefinedOn(
        "email_not_found",
        request<EmailResponse>(
          devMailboxApiPaths.email(encodeURIComponent(id)),
        ),
      );
      return response?.email;
    },

    async waitFor(filter, { timeoutMs, signal } = {}) {
      const response = await resolveUndefinedOn(
        "timeout",
        post<EmailResponse>(
          devMailboxApiPaths.waitForEmail,
          { ...filter, timeoutMs } satisfies WaitForEmailRequest,
          signal,
        ),
      );
      return response?.email;
    },

    async delete(id) {
      const response = await resolveUndefinedOn(
        "email_not_found",
        post<ActionResponse>(devMailboxApiPaths.deleteEmail, {
          id,
        } satisfies DeleteEmailRequest),
      );
      return response !== undefined;
    },

    async clear() {
      await post<ActionResponse>(devMailboxApiPaths.clearEmails, {});
    },
  };
}

function toQueryString(filter: DevMailboxEmailFilter, limit?: number): string {
  const params = new URLSearchParams();
  if (filter.search !== undefined) {
    params.set("search", filter.search);
  }
  if (filter.searchIn !== undefined) {
    params.set("searchIn", filter.searchIn.join(","));
  }
  if (filter.after !== undefined) {
    params.set("after", filter.after);
  }
  if (limit !== undefined) {
    params.set("limit", String(limit));
  }
  const query = params.toString();
  return query === "" ? "" : `?${query}`;
}

async function toApiError(response: Response): Promise<Error> {
  const body: unknown = await response.json().catch(() => undefined);
  if (isApiErrorResponse(body)) {
    return new ApiError(response.status, body.error.code, body.error.message);
  }
  return new Error(`Dev mailbox API responded with ${response.status}.`);
}

function isApiErrorResponse(body: unknown): body is ApiErrorResponse {
  return (
    typeof body === "object" &&
    body !== null &&
    "error" in body &&
    typeof body.error === "object" &&
    body.error !== null &&
    "code" in body.error
  );
}

async function resolveUndefinedOn<T>(
  code: ApiErrorCode,
  promise: Promise<T>,
): Promise<T | undefined> {
  try {
    return await promise;
  } catch (error) {
    if (error instanceof ApiError && error.code === code) {
      return undefined;
    }
    throw error;
  }
}
