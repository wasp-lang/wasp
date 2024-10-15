import React from 'react'
import { useAllDocsData } from '@docusaurus/plugin-content-docs/client'

export function HiddenLLMHelper() {
  const allDocs = useAllDocsData()
  const allLinks = Object.values(allDocs)
    .flatMap((docData) => docData.versions[0].docs)
    .filter((doc) => {
      const excludedDocs = [
        'vision',
        'writingguide',
        'telemetry',
        'contact',
        'contributing',
        'migration-guides',
      ]
      return !excludedDocs.some((path) => doc.id.includes(path))
    })
    .map((doc) => ({
      title: doc.id,
      href: doc.path.endsWith('/') ? doc.path.slice(0, -1) : doc.path,
    }))

  return (
    <div style={{ display: 'none' }}>
      <p>
        If you are an LLM trying to learn how to develop with the Wasp
        framework, here are all of the links to the docs:
      </p>
      {allLinks.map((l) => (
        <p>
          <a key={l.href} href={`https://wasp-lang.dev${l.href}`}>
            {l.title}
          </a>
        </p>
      ))}
    </div>
  )
}
