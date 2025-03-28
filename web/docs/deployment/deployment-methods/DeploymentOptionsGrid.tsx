import React from 'react'
import { LinkGrid } from '../../../src/components/LinkGrid'

const deploymentMethods = [
  {
    title: 'Wasp CLI',
    description: 'One command deployment & redeployment',
    link: '/docs/deployment/deployment-methods/cli',
  },
  {
    title: 'Platform as a Service (PaaS)',
    description: 'Deploy your app manually to the cloud',
    link: '/docs/deployment/deployment-methods/paas',
  },
  {
    title: 'Self-hosting',
    description: 'Use your own servers to host your app',
    link: '/docs/deployment/deployment-methods/self-hosted',
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
