import chalk, { type ChalkInstance } from "chalk";

type LogType = "error" | "warn" | "info" | "success" | "debug" | "fatal";

const logTypeToColorFn: Record<LogType, ChalkInstance> = {
  error: chalk.red,
  warn: chalk.yellow,
  info: chalk.cyan,
  success: chalk.green,
  debug: chalk.gray,
  fatal: chalk.bold.red,
};

export type Logger = ReturnType<typeof createLogger>;

export function createLogger(processName: string) {
  function log(type: LogType, message: string): void {
    console.log(formatLogLine(processName, type, message));
  }

  return {
    info(message: string): void {
      log("info", message);
    },
    error(message: string): void {
      log("error", message);
    },
    warn(message: string): void {
      log("warn", message);
    },
    success(message: string): void {
      log("success", message);
    },
    debug(message: string): void {
      log("debug", message);
    },
    /**
     * Reports an unrecoverable error and stops the current flow. It does NOT log
     * here: it throws a {@link LoggerError} carrying this logger's process name
     * and (optionally) a `cause`, so the top level can pretty-print it with the
     * right prefix once cleanup has unwound. Returns `never`.
     */
    fatal(message: string, options?: ErrorOptions): never {
      throw new LoggerError(processName, message, options);
    },
  };
}

/**
 * An error a logger raised via {@link Logger.fatal}. Keeps the originating
 * process name so the top level can render it like any other log line.
 */
export class LoggerError extends Error {
  constructor(
    public readonly processName: string,
    message: string,
    options?: ErrorOptions,
  ) {
    super(message, options);
  }
}

/** Pretty-prints a {@link LoggerError} (and its cause chain) to stderr. */
export function reportFatalError(error: LoggerError): void {
  const lines = [error.message, ...describeCauseChain(error.cause)];
  for (const line of lines) {
    console.error(formatLogLine(error.processName, "fatal", line));
  }
}

function describeCauseChain(cause: unknown): string[] {
  const lines: string[] = [];
  let current = cause;
  while (current !== undefined && current !== null) {
    if (current instanceof Error) {
      lines.push(`Caused by: ${current.message}`);
      current = current.cause;
    } else {
      lines.push(`Caused by: ${String(current)}`);
      break;
    }
  }
  return lines;
}

function formatLogLine(
  processName: string,
  type: LogType,
  message: string,
): string {
  const colorFn = logTypeToColorFn[type];
  const prefix = `[${processName}:${type}]`;
  return `${colorFn(prefix)} ${message}`;
}
