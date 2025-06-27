import { chalk } from "zx";

export function waspSays(str: string): void {
  console.log(chalk.yellow(`🚀 ${str}`));
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
