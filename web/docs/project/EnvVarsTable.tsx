import React from 'react'
import './EnvVarsTable.css'

// @ts-ignore
import { Required, Optional } from '@site/src/components/Tag'

export function EnvVarsTable({ children }: { children: React.ReactNode }) {
  return (
    <table className="env-vars-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Notes</th>
        </tr>
      </thead>
      <tbody>{children}</tbody>
    </table>
  )
}

export function EnvVar({
  name,
  type,
  isRequired = false,
  note,
  defaultValue,
}: {
  name: string
  type: 'URL' | 'string' | 'number' | 'boolean'
  isRequired?: boolean
  note: string
  defaultValue?: string
}) {
  const requiredQualifier = isRequired ? <Required /> : <Optional />
  return (
    <tr>
      <td>
        <code>{name}</code>
      </td>
      <td>
        <span className="env-var-type">
          <span>{type}</span>
          <span>{requiredQualifier}</span>
        </span>
        {defaultValue && (
          <span className="env-var-default">
            Default: <code>{defaultValue}</code>
          </span>
        )}
      </td>
      <td>{note}</td>
    </tr>
  )
}
