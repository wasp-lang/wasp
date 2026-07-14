import rehypeParse from "rehype-parse";
import rehypeRemark from "rehype-remark";
import remarkDirective from "remark-directive";
import remarkGfm from "remark-gfm";
import remarkStringify from "remark-stringify";
import { unified } from "unified";
import { VFile } from "vfile";
import { MarkdownDocsContext } from "./context";
import { docusaurusHtmlToMdHandlers } from "./docusaurus/docusaurus-html-to-md-handlers";
import { rehypeReduceDocusaurusPageToValidMarkdownContent } from "./docusaurus/rehype-reduce-docusaurus-page";

/**
 * Creates a Docusaurus HTML to markdown processor.
 *
 * Docusaurus renders MDX into HTML with theme-specific wrappers that the default markdown
 * conversion mangles. This processor recognizes them and emits clean markdown.
 */
export function createDocusaurusHtmlToMarkdownProcessor(
  context: MarkdownDocsContext,
): (htmlFile: VFile) => string {
  const docusaurusHtmlToMarkdownProcessor = unified()
    .use(rehypeParse)
    .use(
      rehypeReduceDocusaurusPageToValidMarkdownContent,
      context.skipElementInMarkdownDocsClass,
    )
    .use(rehypeRemark, docusaurusHtmlToMdHandlers)
    .use(remarkGfm)
    .use(remarkDirective)
    .use(remarkStringify, {
      bullet: "-",
      emphasis: "*",
      strong: "*",
      fence: "`",
      fences: true,
      rule: "-",
      listItemIndent: "one",
    });

  return (htmlFile) => {
    const htmlContent = htmlFile.toString();
    const markdownContent = String(
      docusaurusHtmlToMarkdownProcessor.processSync(htmlContent),
    ).trim();
    if (!markdownContent) {
      htmlFile.fail(
        `Generated empty markdown from HTML at "${htmlFile.path}" (${htmlContent.length} bytes). ` +
          `Likely a stray or invalid document.`,
      );
    }
    return markdownContent;
  };
}
