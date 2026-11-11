import { SERVER_ERROR_PATH, type ServerErrorPayload } from "./shared.js";

const SEND_TIMEOUT_MS = 1000;

/**
 * Sends a server error to the Vite dev server running the client, so that it
 * shows up in the browser console. Meant for development only.
 *
 * Never throws: if the client dev server isn't running, the error just stays
 * in the terminal.
 */
export async function sendServerErrorToDevClient(
  clientUrl: string,
  message: string,
): Promise<void> {
  const payload: ServerErrorPayload = { message };
  try {
    await fetch(new URL(SERVER_ERROR_PATH, withTrailingSlash(clientUrl)), {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
      signal: AbortSignal.timeout(SEND_TIMEOUT_MS),
    });
  } catch {
    // The client dev server isn't reachable, nothing to do.
  }
}

function withTrailingSlash(url: string): string {
  return url.endsWith("/") ? url : `${url}/`;
}
