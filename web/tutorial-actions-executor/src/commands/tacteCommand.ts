import { Command, Option } from "@commander-js/extra-typings";

export function createTacteCommand(commandName: string) {
  return new Command(commandName)
    .addOption(
      new Option(
        "--app-name <name>",
        "Name of the app to generate",
      ).makeOptionMandatory(),
    )
    .addOption(
      new Option(
        "--output-dir <path>",
        "Directory where the app will be generated",
      ).default("./.result"),
    )
    .addOption(
      new Option(
        "--tutorial-dir <path>",
        "Directory containing the tutorial MDX files",
      ).makeOptionMandatory(),
    );
}
