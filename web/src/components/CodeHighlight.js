import React from 'react'
import Prism from 'prismjs'
import Highlight from 'prism-react-renderer'
import githubLight from 'prism-react-renderer/themes/github'
import '../css/prismjs-github-theme.css'

githubLight.plain = {
  ...githubLight.plain,
  backgroundColor: '#f5f5f5',
}

export default function CodeHighlight({ language, source }) {
  return (
    <Highlight
      Prism={Prism}
      theme={githubLight}
      code={source}
      language={language}
    >
      {({ className, style, tokens, getLineProps, getTokenProps }) => (
        <pre
          className={className}
          style={{
            borderBottomLeftRadius: '0px',
            borderBottomRightRadius: '0px',
            borderTopLeftRadius: '0px',
            borderTopRightRadius: '0px',
            // margin: '5px',
            ...style,
          }}
        >
          {tokens.map((line, i) => (
            <div key={i} {...getLineProps({ line })}>
              {/* <span>{i + 1}</span> */}
              {line.map((token, key) => (
                <span key={key} {...getTokenProps({ token })} />
              ))}
            </div>
          ))}
        </pre>
      )}
    </Highlight>
  )
}
