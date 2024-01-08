import * as React from 'react'

export const Tag = ({
  color,
  children,
}: React.PropsWithChildren<{
  color: string
}>) => {
  return (
    <span
      style={{
        border: `2px solid ${color}`,
        display: 'inline-block',
        padding: '0.2em 0.4em',
        color: color,
        borderRadius: '0.4em',
        fontSize: '0.8em',
        lineHeight: '1',
        fontWeight: 'bold',
      }}
    >
      {children}
    </span>
  )
}
