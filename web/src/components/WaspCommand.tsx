import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";

interface Props {
  /**
   * The shell commands as you'd type them with the global Wasp CLI, one per
   * line, e.g. `wasp db migrate-dev`. Lines that don't run `wasp` (like
   * `cd my-app` or comments) show up unchanged in every tab.
   *
   * MDX drops the leading whitespace of lines inside a `{...}` expression, and
   * a template literal breaks inside a list item. So write multi-line
   * commands with indented lines, or inside a list, as a string with `\n`:
   * `command={"cd my-app\nwasp start"}`.
   */
  command: string;
}

/**
 * Shows Wasp commands in tabs: one for running the project's own Wasp
 * through its package manager, and one for the global Wasp CLI. The tabs
 * share a `groupId`, so the reader's choice applies across the docs.
 *
 * @example
 * <WaspCommand command="wasp db migrate-dev" />
 */
export default function WaspCommand({ command }: Props): ReactNode {
  return (
    <Tabs groupId="wasp-cli">
      {[...PROJECT_RUNNERS, GLOBAL_CLI_RUNNER].map((runner) => (
        <TabItem key={runner.value} value={runner.value} label={runner.label}>
          <CodeBlock language="shell">
            {command
              .trim()
              .split("\n")
              .map((line) => toRunnerLine(runner, line))
              .join("\n")}
          </CodeBlock>
        </TabItem>
      ))}
    </Tabs>
  );
}

interface Runner {
  value: string;
  label: string;
  /** Turns the arguments of a `wasp` command into the command to run. */
  toShellCommand: (waspArgs: string[]) => string;
}

// The package managers that run the project's own Wasp. To support another
// one, add it here.
const PROJECT_RUNNERS: Runner[] = [
  {
    value: "npm",
    label: "npm",
    toShellCommand: (waspArgs) => {
      const projectScript = getProjectScript(waspArgs);
      if (projectScript) {
        return [
          "npm",
          "run",
          projectScript.name,
          ...withArgsSeparator(projectScript.args),
        ].join(" ");
      }
      if (waspArgs[0] === "new") {
        // There's no project to run Wasp from yet.
        return ["npx", "@wasp/cli@latest", ...waspArgs].join(" ");
      }
      // npm treats the flags after `npm run <script>` as its own, so we
      // always pass Wasp's arguments after `--`.
      return ["npm", "run", "wasp", "--", ...waspArgs].join(" ");
    },
  },
];

const GLOBAL_CLI_RUNNER: Runner = {
  value: "global",
  label: "Global CLI",
  toShellCommand: (waspArgs) => ["wasp", ...waspArgs].join(" "),
};

// The npm scripts that Wasp starters add to package.json.
const PROJECT_SCRIPTS: { name: string; waspArgs: string[] }[] = [
  { name: "dev", waspArgs: ["start"] },
  { name: "preview", waspArgs: ["build", "start"] },
  { name: "build", waspArgs: ["build"] },
];

function getProjectScript(
  waspArgs: string[],
): { name: string; args: string[] } | undefined {
  const projectScript = PROJECT_SCRIPTS.find((script) =>
    script.waspArgs.every((arg, i) => waspArgs[i] === arg),
  );
  if (!projectScript) {
    return undefined;
  }
  const args = waspArgs.slice(projectScript.waspArgs.length);
  // `wasp start db` is a different command than `wasp start`.
  if (projectScript.name === "dev" && args[0] === "db") {
    return undefined;
  }
  return { name: projectScript.name, args };
}

function withArgsSeparator(args: string[]): string[] {
  return args.length > 0 ? ["--", ...args] : [];
}

function toRunnerLine(runner: Runner, line: string): string {
  const match = line.match(/^(\s*)wasp(?:\s+(.*?))?(\s+#.*)?$/);
  if (!match) {
    return line;
  }
  const [, indent, args = "", comment = ""] = match;
  const waspArgs = args.split(/\s+/).filter(Boolean);
  return indent + runner.toShellCommand(waspArgs) + comment;
}
