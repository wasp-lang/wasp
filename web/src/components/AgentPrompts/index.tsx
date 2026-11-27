import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import clsx from "clsx";
import type { ReactNode } from "react";

import type { AgentPluginKind } from "@site/src/lib/analytics";
import { SKIP_IN_MARKDOWN_DOCS_CLASS } from "@site/src/lib/constants";
import Trackable from "../Trackable";

import styles from "./styles.module.css";

type AgentPrompt = {
  label: string;
  caption: string;
  trackKind: AgentPluginKind;
  text: string;
};

const AGENT_PROMPTS = {
  explore: {
    label: "Explain Wasp",
    caption: "Paste into any AI agent that can read web pages.",
    trackKind: "explore_prompt",
    text: `
Read https://wasp.sh/docs.md and https://wasp.sh/llms.txt, then:

1. In under 200 words, explain what Wasp is and list its key features, one line each.
2. Assess how well Wasp suits AI-assisted coding: what its spec file and project structure mean for a coding agent that reads and changes the code, what an agent would find easy, and where it could still go wrong.
3. In 2–3 sentences, compare Wasp with the stack I usually use. Ask me if you don't know it.

Use only the Wasp docs for claims about Wasp, and say so when they don't cover something.
`,
  },
  setup: {
    label: "Set up Wasp",
    caption:
      "Paste into a coding agent (Claude Code, Cursor, Codex, ...) opened in the folder where you want your app.",
    trackKind: "setup_prompt",
    text: `
Read https://wasp.sh/docs/quick-start.md, then set up a new Wasp app for me:

1. Check that my Node.js version meets the quick start's requirement. If it doesn't, offer to install the right version with nvm.
2. Install the Wasp CLI and check that it works with \`wasp version\`.
3. Ask me what I want to build and suggest a starter template (basic, minimal or saas). Then create the project with \`wasp new <name> -t <template>\`.
4. Read the new project's AGENTS.md and follow its setup steps.
5. Start the app with \`wasp start\` in the background and tell me the URL it prints.

Ask me before you install anything globally. On Windows, set everything up inside WSL.
`,
  },
} as const satisfies Record<string, AgentPrompt>;

type AgentPromptId = keyof typeof AGENT_PROMPTS;

// Copyable prompts that people paste into their AI agent. The markdown docs
// skip them: an agent reading a page doesn't need prompts for itself.
export default function AgentPrompts({
  prompts = ["explore", "setup"],
}: {
  prompts?: AgentPromptId[];
}): ReactNode {
  return (
    <div className={clsx(styles.card, SKIP_IN_MARKDOWN_DOCS_CLASS)}>
      {prompts.length === 1 ? (
        <>
          <div className={styles.label}>{AGENT_PROMPTS[prompts[0]].label}</div>
          <AgentPromptBody prompt={AGENT_PROMPTS[prompts[0]]} />
        </>
      ) : (
        <Tabs className={styles.tabs}>
          {prompts.map((id) => (
            <TabItem key={id} value={id} label={AGENT_PROMPTS[id].label}>
              <AgentPromptBody prompt={AGENT_PROMPTS[id]} />
            </TabItem>
          ))}
        </Tabs>
      )}
    </div>
  );
}

function AgentPromptBody({ prompt }: { prompt: AgentPrompt }): ReactNode {
  return (
    <>
      <p className={styles.caption}>{prompt.caption}</p>
      <Trackable event="Agent Plugin: Copy" kind={prompt.trackKind}>
        <CodeBlock language="text">{prompt.text.trim()}</CodeBlock>
      </Trackable>
    </>
  );
}
