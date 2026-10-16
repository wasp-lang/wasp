export type CliCommand =
  | "wasp_new"
  | "wasp_start"
  | "wasp_db"
  | "wasp_deploy"
  | "wasp_build"
  // Any other `wasp` command.
  | "wasp_other"
  // A shell block with no `wasp` command, for example `npm install`.
  | "other_shell";

export type AgentPluginKind =
  // `claude plugin ...` commands.
  | "claude_plugin"
  // `npx skills add wasp-lang/wasp-agent-plugins`.
  | "skills_cli"
  // Any other text that names the plugin, for example a slash command.
  | "plugin_prompt"
  // The example prompt on `/vibe-coding`.
  | "example_prompt";

export type CopyClassification =
  | { name: "Install Command: Copy"; props: Record<string, never> }
  | { name: "Agent Plugin: Copy"; props: { kind: AgentPluginKind } }
  | { name: "CLI Command: Copy"; props: { command: CliCommand } };

const INSTALL_COMMAND = /^npm (i|install) -g @wasp\.sh\/wasp-cli\b/;
const AGENT_PLUGIN_TEXT =
  /claude plugin|npx skills add wasp-lang\/wasp-agent-plugins|wasp-plugin|start-dev-server/;
const WASP_COMMAND = /^wasp (new|start|db|deploy|build)\b/;
const SHELL_LANGUAGES = new Set([
  "shell",
  "sh",
  "bash",
  "zsh",
  "console",
  "shell-session",
]);

export function isShellLanguage(language: string | undefined): boolean {
  return language !== undefined && SHELL_LANGUAGES.has(language.toLowerCase());
}

export function classifyCopiedText(
  text: string,
  { isShellBlock }: { isShellBlock: boolean },
): CopyClassification | null {
  const lines = commandLines(text);

  if (lines.some((line) => INSTALL_COMMAND.test(line))) {
    return { name: "Install Command: Copy", props: {} };
  }

  if (AGENT_PLUGIN_TEXT.test(text)) {
    return {
      name: "Agent Plugin: Copy",
      props: { kind: agentPluginKind(text) },
    };
  }

  if (!isShellBlock) {
    return null;
  }

  for (const line of lines) {
    const match = line.match(WASP_COMMAND);
    if (match) {
      return {
        name: "CLI Command: Copy",
        props: { command: `wasp_${match[1]}` as CliCommand },
      };
    }
  }

  return {
    name: "CLI Command: Copy",
    props: {
      command: lines.some((line) => line.startsWith("wasp "))
        ? "wasp_other"
        : "other_shell",
    },
  };
}

function commandLines(text: string): string[] {
  return text
    .split("\n")
    .map((line) => line.trim().replace(/^[$%]\s+/, ""))
    .filter(
      (line) => line !== "" && !line.startsWith("#") && !line.startsWith("cd "),
    );
}

function agentPluginKind(text: string): AgentPluginKind {
  if (text.includes("claude plugin")) {
    return "claude_plugin";
  }
  if (text.includes("npx skills add")) {
    return "skills_cli";
  }
  return "plugin_prompt";
}
