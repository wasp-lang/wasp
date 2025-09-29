import { chalk } from "zx";

const levels = {
  info: { color: chalk.blue },
  success: { color: chalk.green },
  error: { color: chalk.red },
};

export function log(level: keyof typeof levels, message: string) {
  console.log(
    levels[level].color(`[${level.toUpperCase()}] ${chalk.reset(message)}`),
  );
}
