type LogType = "error" | "warn" | "info" | "success" | "debug";

export function log(processName: string, type: LogType, message: string): void {
  const logTypeToColor: Record<LogType, number> = {
    error: 31,
    warn: 33,
    info: 36,
    success: 32,
    debug: 90,
  };
  const typeColor: number = logTypeToColor[type] ?? 0;

  console.log(
    `\x1b[0m` +
      `\x1b[${typeColor}m[${processName}${
        type ? `:${type}` : ""
      }]\x1b[0m ${message}`
  );
}
