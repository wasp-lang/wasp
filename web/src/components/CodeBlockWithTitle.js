import React, { useState } from 'react'
import CodeBlock from '@theme/CodeBlock'

export default function CodeBlockWithTitle ({ title, children, language, metastring }) {
  return (
    <div className='code-with-header'>
      <div className="code-header">{ title }</div>

      <CodeBlock className={language} metastring={metastring}>
        { children }
      </CodeBlock>
    </div>
  )
}
