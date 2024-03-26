// Copied from 
// https://github.com/redwoodjs/redwood/blob/bd903c5755925ea7174775a2fdaba371b700c910/docs/src/remark/auto-import-tabs.js
// and modified to work with nested tab usage.

const needImports = (tree) => {
  const checkChildren = (children) => {
    for (const child of children) {
      if (child.type === 'jsx' && /^<Tabs\b/.test(child.value)) {
        return true;
      }
      if (child.children && child.children.length > 0) {
        if (checkChildren(child.children)) {
          return true;
        }
      }
    }
    return false;
  };

  return checkChildren(tree.children);
};

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
