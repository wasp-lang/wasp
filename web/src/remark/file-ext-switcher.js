// Copied from
// https://github.com/redwoodjs/redwood/blob/bd903c5755925ea7174775a2fdaba371b700c910/docs/src/remark/file-ext-switcher.js

const visit = require('unist-util-visit')

const plugin = () => {
  let needImport = false

  return (tree, _file) => {
    visit(tree, (node) => {
      if (node.type === 'inlineCode' && /\w\.\{jsx?,tsx?}$/.test(node.value)) {
        needImport = true
        node.type = 'jsx'
        node.value = `<FileExtSwitcher path="${node.value}" />`
      }
    })

    if (needImport) {
      tree.children.unshift({
        type: 'import',
        value:
          "import FileExtSwitcher from '@site/src/components/FileExtSwitcher'",
      })
    }
  }
}

module.exports = plugin
