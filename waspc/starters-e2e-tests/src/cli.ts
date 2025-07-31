export type WaspCliCommand = "wasp" | "wasp-cli";

type StartersHeadlessTestsArgs = {
  waspCliCommand: WaspCliCommand;
};

export function parseArgs(args: string[]): StartersHeadlessTestsArgs {
  const [_node, _script, ...userArgs] = args;

  if (userArgs.length === 0) {
    console.error('Please provide Wasp CLI command ("wasp" or "wasp-cli")');
    process.exit(1);
  }

  const waspCliCommand = userArgs[0];

  if (waspCliCommand !== "wasp" && waspCliCommand !== "wasp-cli") {
    console.error('Wasp CLI command must be either "wasp" or "wasp-cli"');
    process.exit(1);
  }

  return { waspCliCommand };
}
