const visit = require('unist-util-visit')
const docsVersions = require('../../versions.json')

const latestWaspVersion = docsVersions[0]

const replacements = [
  {
    search: /{latestWaspVersion}/g,
    replace: `^${latestWaspVersion}`,
  },
]

function searchAndReplace(tree) {
  visit(tree, (node) => {
    // NOTE: For now we only replace in code blocks to keep
    // the search and replace logic simple.
    if (node.type === 'code') {
      replacements.forEach(({ search, replace }) => {
        node.value = node.value.replace(search, replace)
      })
    }
  })
}

module.exports = () => searchAndReplace
