import React from 'react'
import './DeploymentOptionsGrid.css'

export function DeploymentOptionsGrid() {
  const deploymentMethods = [
    {
      title: 'Wasp CLI',
      description: 'One command deployment & redeployment',
      linkToDocs: '/docs/advanced/deployment/cli',
    },
    {
      title: 'Platform as a Service (PaaS)',
      description: 'Deploy your app manually to the cloud',
      linkToDocs: '/docs/deployment/deployment-methods/paas',
    },
    {
      title: 'Self-hosting',
      description: 'Use your own servers to host your app',
      linkToDocs: '/docs/deployment/deployment-methods/self-hosted',
    },
  ]
  return (
    <>
      <div className="deployment-methods-grid">
        {deploymentMethods.map((deploymentMethod) => (
          <DeploymentOptionBox
            title={deploymentMethod.title}
            description={deploymentMethod.description}
            linkToDocs={deploymentMethod.linkToDocs}
          />
        ))}
      </div>
      <p className="deployment-methods-info">
        <small>Click on each deployment method for more details.</small>
      </p>
    </>
  )
}

function DeploymentOptionBox({
  linkToDocs,
  title,
  description,
}: {
  linkToDocs: string
  title: string
  description: string
}) {
  return (
    <a href={linkToDocs} className="deployment-method-box">
      <h3>{title} »</h3>
      <p>{description}</p>
    </a>
  )
}
