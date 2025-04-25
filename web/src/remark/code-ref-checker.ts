import type { Root } from 'mdast'
import type { Plugin } from 'unified'
import { visit } from 'unist-util-visit'

const plugin: Plugin<[], Root> = () => {
  return (tree, _file) => {
    visit(tree, 'code', (node) => {
      if (!node.meta?.includes('ref=')) {
        return
      }

      // parse node meta
      // source starts relative from wasp monorepo dir
      // example:
      // ref={source="./"}

      // fetch source code

      // compare source code with tree node code
    })
  }
}

export default plugin
