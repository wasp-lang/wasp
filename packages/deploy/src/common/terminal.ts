import { chalk } from "zx";

export function waspSays(str: string): void {
  const formattedStr = str
    .split("\n")
    .map((line) => `🚀 ${line}`)
    .join("\n");
  console.log(chalk.yellow(formattedStr));
}

export function waspInfo(str: string): void {
  const formattedStr = str
    .split("\n")
    .map((line) => `👉 ${line}`)
    .join("\n");
  console.log(chalk.blue(formattedStr));
}

export function displayWaspRocketImage(): void {
  // Escaping backslashes makes it look weird here, but it works in console.
  const asciiArt = `

                    __
                   // \\
                   \\\\_/ //
             _    -(||)(')
            \\ \\____///_____
   #########[==__DEPLOYED__}
            /_/

  `;
  console.log(asciiArt);
}
