import { visit } from 'unist-util-visit'

const plugin = () => {
  let needImport = false

  return (tree, _file) => {
    visit(tree, (node, _index, parent) => {
      if (node.type === 'inlineCode' && /\w\.\{jsx?,tsx?}$/.test(node.value)) {
        needImport = true
        const pathValue = `${node.value}`

        node.type =
          parent.type === 'paragraph'
            ? 'mdxJsxTextElement'
            : 'mdxJsxFlowElement'
        node.name = 'FileExtSwitcher'
        node.attributes = [
          {
            type: 'mdxJsxAttribute',
            name: 'path',
            value: pathValue,
          },
        ]
        node.children = []
        node.data = {
          _mdxExplicitJsx: true,
        }
      }
    })

    if (needImport) {
      tree.children.unshift({
        type: 'mdxjsEsm',
        value:
          "import FileExtSwitcher from '@site/src/components/FileExtSwitcher'",
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
                      name: 'FileExtSwitcher',
                    },
                  },
                ],
                source: {
                  type: 'Literal',
                  value: '@site/src/components/FileExtSwitcher',
                  raw: "'@site/src/components/FileExtSwitcher'",
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
}

export default plugin
