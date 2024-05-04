// Inspired by code in
// https://github.com/facebook/docusaurus/blob/main/packages/docusaurus-remark-plugin-npm2yarn/src/index.ts

// Searching only the root node was insufficient when Tabs had been combined with admonition blocks.
// Doing a deep search for the Tabs element is a bit more expensive, but this is build-time code, so
// it's not a big deal. A false positive is also not really a big deal.
const containsTabsJSXElement = (node) => {
  if (node.type === 'mdxJsxFlowElement' && node.name === 'Tabs') {
    return true
  }
  if (!node.children || !Array.isArray(node.children)) {
    return false
  }
  return node.children.some(containsTabsJSXElement)
}

const plugin = () => (tree, _file) => {
  if (containsTabsJSXElement(tree)) {
    // Add `import` nodes to the top of the parsed file
    tree.children.unshift({
      type: 'mdxjsEsm',
      value:
        "import Tabs from '@theme/Tabs'\nimport TabItem from '@theme/TabItem'",
      data: {
        estree: {
          type: 'Program',
          body: [
            {
              type: 'ImportDeclaration',
              specifiers: [
                {
                  type: 'ImportDefaultSpecifier',
                  local: {
                    type: 'Identifier',
                    name: 'Tabs',
                  },
                },
              ],
              source: {
                type: 'Literal',
                value: '@theme/Tabs',
                raw: "'@theme/Tabs'",
              },
            },
            {
              type: 'ImportDeclaration',
              specifiers: [
                {
                  type: 'ImportDefaultSpecifier',
                  local: {
                    type: 'Identifier',
                    name: 'TabItem',
                  },
                },
              ],
              source: {
                type: 'Literal',
                value: '@theme/TabItem',
                raw: "'@theme/TabItem'",
              },
            },
          ],
          sourceType: 'module',
          comments: [],
        },
      },
    })
  }
}

export default plugin
