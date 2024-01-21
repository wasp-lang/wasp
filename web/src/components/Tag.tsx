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

// Used to mark something as internal to
// Wasp and not to be used by the user.
export function Internal() {
  return <Tag color="#0b62f5">internal</Tag>
}

// Used to mark something as required e.g. required
// fields in Wasp file.
export function Required() {
  return <Tag color="#f59e0b">required</Tag>
}
