import { chalk } from "zx";

const colors = {
  info: chalk.blue,
  error: chalk.red,
};

export function log(level: keyof typeof colors, message: string) {
  console.log(
    colors[level](`[${level.toUpperCase()}] ${chalk.reset(message)}`),
  );
}
