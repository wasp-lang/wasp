import type { Nodes, Root } from "mdast";
import type {} from "mdast-util-mdx"; // empty type-only import to register mdx types into mdast
import type { Plugin } from "unified";
import { makeImports } from "./util/make-imports";

const COMPONENT_NAME = "LastCheckedWithVersionsNotice";

/**
 * Injects the {@link COMPONENT_NAME} component just below a guide's title when
 * the frontmatter declares `last_checked_with_versions`.
 *
 * To keep the notice consistently below the title, we place it after a leading
 * inline title when present. Otherwise the title comes from frontmatter and
 * Docusaurus always renders it above the content, so we leave the notice at the top.
 *
 * Docusaurus runs its `contentTitle` plugin before this one, so an inline `# h1`
 * is no longer a `heading` node by the time we see it. It gets wrapped into an
 * `mdxJsxFlowElement` named `header`, so we match that too.
 */
const lastCheckWithVersionsPlugin: Plugin<[], Root> = () => {
  return (tree, file) => {
    const frontMatter = file.data.frontMatter as
      | { last_checked_with_versions?: unknown }
      | undefined;

    if (!frontMatter?.last_checked_with_versions) {
      return;
    }

    const lastCheckWithVersionsNoticeNode: Nodes = {
      type: "mdxJsxFlowElement",
      name: COMPONENT_NAME,
      attributes: [],
      children: [],
    };

    const titleIndex = tree.children.findIndex(
      (node) =>
        (node.type === "heading" && node.depth === 1) ||
        (node.type === "mdxJsxFlowElement" && node.name === "header"),
    );
    const insertIndex = titleIndex === -1 ? 0 : titleIndex + 1;

    tree.children.splice(insertIndex, 0, lastCheckWithVersionsNoticeNode);
    tree.children.unshift(
      makeImports([
        {
          from: `@site/src/components/${COMPONENT_NAME}`,
          importDefaultAs: COMPONENT_NAME,
        },
      ]),
    );
  };
};

export default lastCheckWithVersionsPlugin;
