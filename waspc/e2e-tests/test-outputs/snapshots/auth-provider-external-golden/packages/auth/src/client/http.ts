import { getClientRuntime } from "./runtime.js";

/**
 * The error the forms and actions throw, shaped like Wasp's client
 * `WaspHttpError`: `message` is the server's message, `data` the whole
 * response body -- so `error.data?.data?.message` reads the detail exactly
 * as the in-tree forms did.
 */
export class WaspAuthClientError extends Error {
  public statusCode: number;
  public data: unknown;

  constructor(statusCode: number, message: string, data: unknown) {
    super(message);
    this.name = "WaspAuthClientError";
    this.statusCode = statusCode;
    this.data = data;
  }
}

export async function post<T = Record<string, unknown>>(
  path: string,
  body: unknown,
): Promise<T> {
  const response = await fetch(`${getClientRuntime().apiUrl}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  const data = (await response.json().catch(() => ({}))) as Record<
    string,
    unknown
  >;
  if (!response.ok) {
    const message =
      typeof data.message === "string" ? data.message : response.statusText;
    throw new WaspAuthClientError(response.status, message, data);
  }
  return data as T;
}
