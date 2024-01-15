import React from 'react'
import ReactTooltip from 'react-tooltip'
import { Clipboard } from 'react-feather'

const copyToClipboard = (text) => {
  navigator.clipboard.writeText(text)
}

const InstallCmd = () => {
  const code = 'curl -sSL https://get.wasp-lang.dev/installer.sh | sh'

  return (
    <div
      className="cursor-pointer rounded border border-yellow-500/75 text-sm text-neutral-500"
      data-tip
      data-event="click"
      data-event-off="click"
      data-delay-hide="2000"
    >
      <pre
        className={`
          flex justify-between gap-4
        `}
      >
        <strong>
          <code>
            <span className="select-none">$ </span>
            {code}
          </code>
        </strong>
        <Clipboard size={18} />
      </pre>
      <ReactTooltip
        place="top"
        effect="float"
        backgroundColor="#eab307"
        textColor="white"
        afterShow={() => copyToClipboard(code)}
      >
        <b>Copied to clipboard!</b>
      </ReactTooltip>
    </div>
  )
}

export default InstallCmd
