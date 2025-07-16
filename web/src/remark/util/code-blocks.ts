import type * as md from "mdast";
import { formatCode, FormatCodeOptions } from "./prettier";

/**
 * A code block's meta string format is similar to HTML attributes: a
 * space-separated list of either a key-value pair `name="value"`, or a boolean
 * key `name` without a value.
 *
 * The underlying implementation here is fairly simple, only supporting double
 * quotes for values, and not allowing for escaped quotes or spaces.
 */
export function codeBlockMetaStringToMap(
  metaString: string,
): Map<string, string | undefined> {
  const result = new Map<string, string | undefined>();

  if (!metaString) return result;

  for (const [, key, value] of metaString.matchAll(
    // Regex to match key-value pairs in a meta string
    // Matches `key="value"` or `key`
    // Groups:
    // - Match 1: key
    // - Match 2: value (optional, if present)
    /([^\s=]+)(?:="([^"]*)")?/g,
  )) {
    result.set(key, value);
  }
  return result;
}

export function codeBlockMetaStringFromMap(
  meta: Map<string, string | undefined>,
): string {
  return Array.from(meta.entries(), ([key, value]) =>
    value ? `${key}="${value}"` : key,
  ).join(" ");
}

/**
 * Creates a "check fn" for `unist-util-visit` that checks if a node is a code
 * block with a specific meta flag.
 */
export function makeCheckForCodeWithMeta(metaFlagName: string) {
  return (node: md.Nodes): node is md.Code & { meta: string } =>
    node.type === "code" &&
    node.meta &&
    codeBlockMetaStringToMap(node.meta).has(metaFlagName);
}

/**
 * Checks that the code block's language is supported. If not, it throws an error.
 */
export function assertCodeBlockIsInLanguage<
  Language extends string,
  CodeNode extends md.Code,
>(
  node: CodeNode,
  supportedLanguages: readonly Language[],
): asserts node is CodeNode & { lang: Language } {
  const throwLangError = (errorMessage: string) => {
    const solutionMessage = `Expected code block to be in one of the following languages: ${supportedLanguages.join(", ")}.`;
    throw new Error([errorMessage, solutionMessage].join("\n"));
  };

  if (!node.lang) {
    throwLangError("No language specified.");
  }

  if (!(supportedLanguages as readonly string[]).includes(node.lang)) {
    throwLangError(`Found unexpected language: ${node.lang}.`);
  }
}

export async function formatCodeBlock(
  node: md.Code,
  options: FormatCodeOptions,
): Promise<md.Code> {
  const newCode = await formatCode(node.value, options);
  return { ...node, value: newCode };
}
