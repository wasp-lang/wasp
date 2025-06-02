import type * as md from "mdast";
import { formatCode, FormatCodeOptions } from "./prettier";

const CODE_BLOCK_META_REGEX = /([^\s=]+)(?:="([^"]*)")?/g;
export function parseCodeBlockMetaString(
  metaString: string,
): Map<string, string | undefined> {
  const result = new Map<string, string | undefined>();

  if (!metaString) return result;

  for (const [, key, value] of metaString.matchAll(CODE_BLOCK_META_REGEX)) {
    result.set(key, value);
  }
  return result;
}

export function stringifyCodeBlockMetaString(
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
    parseCodeBlockMetaString(node.meta).has(metaFlagName);
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
