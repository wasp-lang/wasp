import {
  SERVER_ERROR_CONSOLE_PREFIX,
  SERVER_ERROR_EVENT,
  type ServerErrorPayload,
} from "./shared.js";

/**
 * Logs the errors reported by the Wasp server in the browser console. Only
 * does something in development, where Vite's HMR channel is available.
 */
export function logServerErrorsInConsole(): void {
  import.meta.hot?.on(SERVER_ERROR_EVENT, ({ message }: ServerErrorPayload) => {
    // A separate argument, so `%` sequences in the message are not treated
    // as format specifiers.
    console.error(SERVER_ERROR_CONSOLE_PREFIX, message);
  });
}
