import React from 'react'
import ReactTooltip from 'react-tooltip'
import { Clipboard } from 'react-feather'


const copyToClipboard = (code) => {
  navigator.clipboard.writeText(code)
}

const InstallCmd = () => {

  const code = 'curl -sSL https://get.wasp-lang.dev/installer.sh | sh'
  
  return (
    <div
      className='cursor-pointer text-neutral-500 border border-yellow-500/75 rounded'
      data-tip
      data-event='click'
      data-event-off='click'
      data-delay-hide='2000'
    >
      <pre
        className={`
          flex justify-between gap-4
        `}
      >
        <code>
          <span className='select-none'>$ </span>
          {code}
        </code>
        <Clipboard size={18} />
      </pre>
      <ReactTooltip
        place='top'
        effect='float'
        backgroundColor='#eab307'
        textColor='white'
        afterShow={() => copyToClipboard(code)}
      >
        <b>Copied to clipboard!</b>
      </ReactTooltip>
    </div>
  )
}

export default InstallCmd
