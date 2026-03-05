import chalk, { type ChalkInstance } from "chalk";

export class CLIError extends Error {
  constructor(
    public readonly logger: Logger,
    message: string,
  ) {
    super(message);
  }
}

type LogType = "error" | "warn" | "info" | "success" | "debug";

export type Logger = ReturnType<typeof createLogger>;

export function createLogger(processName: string) {
  const logTypeToColorFn: Record<LogType, ChalkInstance> = {
    error: chalk.red,
    warn: chalk.yellow,
    info: chalk.cyan,
    success: chalk.green,
    debug: chalk.gray,
  };

  function log(type: LogType, message: string): void {
    const colorFn = logTypeToColorFn[type];
    const prefix = `[${processName}:${type}]`;

    console.log(`${colorFn(prefix)} ${message}`);
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
    fatal(message: string): never {
      throw new CLIError(this, message);
    },
  };
}
