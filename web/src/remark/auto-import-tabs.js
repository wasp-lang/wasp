// Copied from https://github.com/redwoodjs/redwood/tree/main

const needImports = (tree) =>
  tree.children.some(
    (child) => child.type === 'jsx' && /^<Tabs\b/.test(child.value)
  )

const plugin = () => (tree, _file) => {
  if (needImports(tree)) {
    // Add `import` nodes to the top of the parsed file
    tree.children.unshift({
      type: 'import',
      value: "import Tabs from '@theme/Tabs'",
    })
    tree.children.unshift({
      type: 'import',
      value: "import TabItem from '@theme/TabItem'",
    })
  }
}

module.exports = plugin