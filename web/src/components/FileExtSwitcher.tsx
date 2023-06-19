// Copied from 
// https://github.com/redwoodjs/redwood/blob/bd903c5755925ea7174775a2fdaba371b700c910/docs/src/components/FileExtSwitcher.tsx 

import * as React from 'react'

import { useStorageSlot } from '@docusaurus/theme-common'

interface Props {
  path: string
}

/**
 * Takes a path on the form web/src/layouts/BlogLayout/BlogLayout.{js,tsx} and
 * replaces the end part, {js,tsx}, with the correct file extension depending
 * on what language the user has selected for the code blocks
 */
export default function FileExtSwitcher({ path }: Props) {
  const [jsTs] = useStorageSlot('docusaurus.tab.js-ts')

  const extensionStart = path.lastIndexOf('{')
  const extensions = path.slice(extensionStart + 1, path.length - 1)
  const ts = extensions.split(',')[1]
  const pathWithoutExt = path.slice(0, extensionStart)

  return <code>{pathWithoutExt + (jsTs === 'js' ? 'js' : ts)}</code>
}
