import CodeBlock from "@theme/CodeBlock";

import styles from "./styles.module.css";

type LLMPromptProps = {
  label: string;
  language?: string;
  children: string;
};

export default function LLMPrompt({
  label,
  language = "text",
  children,
}: LLMPromptProps) {
  const prompt = String(children).trim();

  return (
    <section className={styles.promptCard}>
      <div className={styles.header}>{label}</div>
      <CodeBlock language={language}>{prompt}</CodeBlock>
    </section>
  );
}
