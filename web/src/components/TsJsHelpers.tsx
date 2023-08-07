// Copied from (and adapted)
// https://github.com/redwoodjs/redwood/blob/bd903c5755925ea7174775a2fdaba371b700c910/docs/src/components/ShowForTs.tsx

import * as React from 'react'

import { useStorageSlot } from '@docusaurus/theme-common'
import MDXContent from '@theme/MDXContent'

interface Props {
  children: React.ReactNode
}

/**
 * Only renders this block if user has selected TS in the codeblocks
 * @Note leave a blank space after opening the tag e.g.
 *
 * @example
 * <ShowForTs>
 * // {blank space}
 * ### Mdx Formatted content
 * </ShowForTs>
 * **/
export function ShowForTs({ children }: Props) {
  const [jsTs] = useStorageSlot('docusaurus.tab.js-ts')
  return jsTs === 'ts' && <MDXContent>{children}</MDXContent>
}


/**
 * Only renders this block if user has selected JS in the codeblocks
 * @Note leave a blank space after opening the tag e.g.
 *
 * @example
 * <ShowForJs>
 * // {blank space}
 * ### Mdx Formatted content
 * </ShowForJs>
 * **/
export function ShowForJs({ children }: Props) {
  const [jsTs] = useStorageSlot('docusaurus.tab.js-ts')
  return jsTs === 'js' && <MDXContent>{children}</MDXContent>
}

