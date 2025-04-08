import { LinkGrid } from '@site/src/components/LinkGrid'
import React from 'react'

const deploymentMethods = [
  {
    title: 'Wasp CLI',
    description: 'One command deployment & redeployment',
    linkTo: './cli',
  },
  {
    title: 'Platform as a Service (PaaS)',
    description: 'Deploy your app manually to the cloud',
    linkTo: './paas',
  },
  {
    title: 'Self-hosting',
    description: 'Use your own servers to host your app',
    linkTo: './self-hosted',
  },
]

export function DeploymentOptionsGrid() {
  return (
    <LinkGrid
      links={deploymentMethods}
      caption="Click on each deployment method for more details."
    />
  )
}
