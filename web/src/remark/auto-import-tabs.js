// Copied from
// https://github.com/redwoodjs/redwood/blob/bd903c5755925ea7174775a2fdaba371b700c910/docs/src/remark/auto-import-tabs.js 

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
