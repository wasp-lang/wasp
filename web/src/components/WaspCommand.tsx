import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";

interface Props {
  /**
   * The shell commands as you'd type them with the global Wasp CLI, one per
   * line, e.g. `wasp db migrate-dev`. A Wasp command can start with env vars
   * (`REACT_APP_X=1 wasp deploy fly deploy`) and continue on the next lines
   * with a trailing `\`. Lines that don't run `wasp` (like `cd my-app` or
   * comments) show up unchanged in every tab.
   *
   * MDX drops the leading whitespace of lines inside a `{...}` expression, and
   * a template literal breaks inside a list item. So write multi-line
   * commands with indented lines, or inside a list, as a string with `\n`:
   * `command={"cd my-app\nwasp start"}`.
   *
   * Throws at build time when a command isn't one we can show both ways, e.g.
   * an unknown Wasp command or one already written as `npm run wasp`.
   */
  command: string;
}

/**
 * Shows Wasp commands in tabs: one for running the project's own Wasp
 * through npm, and one for the global Wasp CLI. The tabs share a `groupId`, so
 * the reader's choice applies across the docs.
 *
 * @example
 * <WaspCommand command="wasp db migrate-dev" />
 */
export default function WaspCommand({ command }: Props): ReactNode {
  const shellLines = parseShellLines(command);
  return (
    <Tabs groupId="wasp-cli">
      {[NPM_RUNNER, GLOBAL_CLI_RUNNER].map((runner) => (
        <TabItem key={runner.value} value={runner.value} label={runner.label}>
          <CodeBlock language="shell">
            {shellLines.map((line) => renderShellLine(runner, line)).join("\n")}
          </CodeBlock>
        </TabItem>
      ))}
    </Tabs>
  );
}

interface Runner {
  value: string;
  label: string;
  /**
   * Turns a Wasp command into what to type instead of `wasp <args>` on its
   * first line.
   */
  toShellCommand: (waspCommand: WaspCommandLine) => string;
}

const NPM_RUNNER: Runner = {
  value: "npm",
  label: "npm",
  toShellCommand: ({ firstLineArgs, args }) => {
    const projectScript = findProjectScript(args);
    if (projectScript) {
      const scriptArgs = firstLineArgs.slice(projectScript.waspArgs.length);
      // npm reads the flags after `npm run <script>` as its own, so we pass
      // Wasp's arguments after `--`. They may all be on the next lines.
      const hasScriptArgs = args.length > projectScript.waspArgs.length;
      return [
        "npm run",
        projectScript.name,
        ...(hasScriptArgs ? ["--", ...scriptArgs] : []),
      ].join(" ");
    }
    if (args[0] === "new") {
      // There's no project to run Wasp from yet.
      return ["npx @wasp/cli@latest", ...firstLineArgs].join(" ");
    }
    return ["npm run wasp --", ...firstLineArgs].join(" ");
  },
};

const GLOBAL_CLI_RUNNER: Runner = {
  value: "global",
  label: "Global CLI",
  toShellCommand: ({ firstLineArgs }) => ["wasp", ...firstLineArgs].join(" "),
};

interface ProjectScript {
  name: string;
  waspArgs: string[];
}

// The npm scripts that Wasp starters add to package.json.
const PROJECT_SCRIPTS: ProjectScript[] = [
  { name: "dev", waspArgs: ["start"] },
  { name: "preview", waspArgs: ["build", "start"] },
  { name: "build", waspArgs: ["build"] },
];

/**
 * The project script that runs the given Wasp command, if any. The longest
 * match wins, so `wasp build start` runs `preview`, not `build`. A script
 * only matches when the rest of the command are its options, so `wasp start
 * db` isn't `dev`.
 */
function findProjectScript(args: string[]): ProjectScript | undefined {
  return PROJECT_SCRIPTS.filter(
    (script) =>
      script.waspArgs.every((arg, i) => args[i] === arg) &&
      isOptionOrEnd(args[script.waspArgs.length]),
  ).sort((a, b) => b.waspArgs.length - a.waspArgs.length)[0];
}

function isOptionOrEnd(arg: string | undefined): boolean {
  return arg === undefined || arg.startsWith("-");
}

interface WaspCliCommand {
  waspArgs: string[];
  takesArgs: boolean;
}

// The Wasp CLI's commands (see `waspc/cli/exe/Main.hs`), and whether they
// take more arguments.
const WASP_CLI_COMMANDS: WaspCliCommand[] = [
  { waspArgs: ["build", "start"], takesArgs: true },
  { waspArgs: ["build"], takesArgs: false },
  { waspArgs: ["clean"], takesArgs: false },
  { waspArgs: ["compile"], takesArgs: false },
  { waspArgs: ["completion"], takesArgs: false },
  { waspArgs: ["db"], takesArgs: true },
  { waspArgs: ["deploy"], takesArgs: true },
  { waspArgs: ["deps"], takesArgs: false },
  { waspArgs: ["dockerfile"], takesArgs: false },
  { waspArgs: ["doctor"], takesArgs: false },
  { waspArgs: ["new"], takesArgs: true },
  { waspArgs: ["news"], takesArgs: false },
  { waspArgs: ["show"], takesArgs: true },
  { waspArgs: ["start"], takesArgs: true },
  { waspArgs: ["studio"], takesArgs: false },
  { waspArgs: ["telemetry"], takesArgs: false },
  { waspArgs: ["uninstall"], takesArgs: true },
  { waspArgs: ["version"], takesArgs: false },
];

type ShellLine =
  | { kind: "wasp"; command: WaspCommandLine }
  | { kind: "other"; text: string };

interface WaspCommandLine {
  indent: string;
  /** The env var assignments before `wasp`, e.g. `REACT_APP_X=1 `. */
  envPrefix: string;
  /** The arguments on the line that runs `wasp`. */
  firstLineArgs: string[];
  /** All the arguments, including those on continuation lines. */
  args: string[];
  /** The rest of the first line: a trailing `\` and a comment, if any. */
  suffix: string;
}

const ENV_ASSIGNMENTS_PATTERN = String.raw`(?:[A-Za-z_][A-Za-z0-9_]*=\S+\s+)*`;
const WASP_LINE_REGEX = new RegExp(
  String.raw`^(\s*)(${ENV_ASSIGNMENTS_PATTERN})wasp(?:\s+(.*?))?(\s*\\)?(\s+#.*)?$`,
);
// A `wasp` command that doesn't start its line, e.g. `cd app && wasp build`.
const INLINE_WASP_REGEX = new RegExp(
  String.raw`(?:&&|\|\||;|\|)\s*${ENV_ASSIGNMENTS_PATTERN}wasp(?:\s|$)`,
);
// Commands already written for npm. WaspCommand renders the npm form itself.
const NPM_FORM_REGEX =
  /\bnpx\s+wasp\b|\bnpm\s+run\s+(?:wasp|dev|build|preview)\b/;

function parseShellLines(command: string): ShellLine[] {
  const lines = command.trim().split("\n");
  const shellLines: ShellLine[] = [];
  let i = 0;
  while (i < lines.length) {
    const match = lines[i].match(WASP_LINE_REGEX);
    if (!match) {
      assertNoMissedWaspCommand(lines[i], command);
      shellLines.push({ kind: "other", text: lines[i] });
      i++;
      continue;
    }

    const [
      ,
      indent,
      envPrefix,
      argsText = "",
      continuation = "",
      comment = "",
    ] = match;
    // Continuation lines stay as they are, but their arguments decide which
    // project script runs the command.
    const continuationLines: string[] = [];
    let isContinued = continuation !== "";
    while (isContinued && i + 1 < lines.length) {
      i++;
      continuationLines.push(lines[i]);
      isContinued = lines[i].trimEnd().endsWith("\\");
    }
    const firstLineArgs = splitArgs(argsText);
    const waspCommand: WaspCommandLine = {
      indent,
      envPrefix,
      firstLineArgs,
      args: [
        ...firstLineArgs,
        ...continuationLines.flatMap((line) =>
          splitArgs(line.trimEnd().replace(/\\$/, "")),
        ),
      ],
      suffix: continuation + comment,
    };
    assertKnownWaspCommand(waspCommand.args, command);
    shellLines.push(
      { kind: "wasp", command: waspCommand },
      ...continuationLines.map((text): ShellLine => ({ kind: "other", text })),
    );
    i++;
  }

  if (!shellLines.some((line) => line.kind === "wasp")) {
    throw new Error(`WaspCommand got no \`wasp\` command in: ${command}`);
  }
  return shellLines;
}

function renderShellLine(runner: Runner, line: ShellLine): string {
  if (line.kind === "other") {
    return line.text;
  }
  const { indent, envPrefix, suffix } = line.command;
  return indent + envPrefix + runner.toShellCommand(line.command) + suffix;
}

function splitArgs(argsText: string): string[] {
  return argsText.split(/\s+/).filter(Boolean);
}

function assertKnownWaspCommand(args: string[], source: string): void {
  if (args.length === 0) {
    // `wasp` alone prints the help.
    return;
  }
  const waspCliCommand = WASP_CLI_COMMANDS.filter((cliCommand) =>
    cliCommand.waspArgs.every((arg, i) => args[i] === arg),
  ).sort((a, b) => b.waspArgs.length - a.waspArgs.length)[0];
  if (!waspCliCommand) {
    throw new Error(
      `WaspCommand got an unknown Wasp command \`wasp ${args[0]}\` in: ${source}`,
    );
  }
  if (
    !waspCliCommand.takesArgs &&
    args.length > waspCliCommand.waspArgs.length
  ) {
    throw new Error(
      `WaspCommand got arguments for \`wasp ${waspCliCommand.waspArgs.join(" ")}\`, which takes none, in: ${source}`,
    );
  }
}

function assertNoMissedWaspCommand(line: string, source: string): void {
  if (line.trimStart().startsWith("#")) {
    return;
  }
  if (NPM_FORM_REGEX.test(line)) {
    throw new Error(
      `WaspCommand renders the npm form itself, so write the command with \`wasp\`, in: ${source}`,
    );
  }
  if (INLINE_WASP_REGEX.test(line)) {
    throw new Error(
      `WaspCommand only recognizes \`wasp\` at the start of a line, so put it on its own line, in: ${source}`,
    );
  }
  if (/^\s*\$\s/.test(line)) {
    throw new Error(
      `WaspCommand shows commands without a \`$\` prompt, in: ${source}`,
    );
  }
}
