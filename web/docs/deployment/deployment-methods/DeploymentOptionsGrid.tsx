import React from 'react'
import './DeploymentOptionsGrid.css'

export function DeploymentOptionsGrid() {
  const deploymentMethods = [
    {
      title: 'Using Wasp CLI',
      description: 'One command deployment & redeployment',
      linkToDocs: '/docs/advanced/deployment/cli',
    },
    {
      title: 'Deploying Manually',
      description: 'Build the app and deploy it manually',
      linkToDocs: '/docs/advanced/deployment/manually',
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
