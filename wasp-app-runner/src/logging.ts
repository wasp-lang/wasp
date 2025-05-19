type LogType = "error" | "warn" | "info" | "success" | "debug";

export function createLogger(processName: string) {
  const logTypeToColor: Record<LogType, number> = {
    error: 31,
    warn: 33,
    info: 36,
    success: 32,
    debug: 90,
  };

  function log(type: LogType, message: string): void {
    const typeColor: number = logTypeToColor[type] ?? 0;

    console.log(
      `\x1b[0m` +
        `\x1b[${typeColor}m[${processName}${
          type ? `:${type}` : ""
        }]\x1b[0m ${message}`
    );
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
  };
}
