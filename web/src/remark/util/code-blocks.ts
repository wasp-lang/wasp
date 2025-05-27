import escapeStringRegexp from "escape-string-regexp";
import type * as md from "mdast";

/**
 * Creates a "check fn" for `unist-util-visit` that checks if a node is a code
 * block with a specific meta flag.
 */
export function makeCheckForCodeWithMeta(metaFlag: string) {
  // We wrap the string in \b to denote a word boundary, so we don't match it
  // as a substring of another word.
  const metaFlagRegexp = new RegExp(
    String.raw`\b${escapeStringRegexp(metaFlag)}\b`,
  );

  return (node: md.Nodes): node is md.Code & { meta: string } =>
    node.type === "code" && node.meta && metaFlagRegexp.test(node.meta);
}

/**
 * Checks that the code block's language is supported. If not, it throws an error.
 */
export function assertSupportedLanguage<
  Language extends string,
  CodeNode extends md.Code,
>(
  node: CodeNode,
  supportedLanguages: Set<Language>,
): asserts node is CodeNode & { lang: Language } {
  let errorMessage: string | undefined;

  if (!node.lang) {
    errorMessage = "No language specified.";
  } else if (!(supportedLanguages as Set<string>).has(node.lang)) {
    errorMessage = `Unsupported language: ${node.lang}.`;
  }

  if (errorMessage) {
    const solutionMessage = `Please use one of: ${[...supportedLanguages].join(", ")}`;
    throw new Error([errorMessage, solutionMessage].join("\n"));
  }
}
