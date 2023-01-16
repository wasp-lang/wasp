import { $, echo, cd } from 'zx'
import crypto from 'crypto'
import * as tomlHelpers from '../helpers/tomlFileHelpers.js'
import { DeploymentInfo, IDeploymentInfo } from '../DeploymentInfo.js'
import { IGlobalOptions } from '../IGlobalOptions.js'
import { buildDirExists, cdToClientBuildDir, cdToServerBuildDir, lazyInit } from '../helpers/helpers.js'

// TOOD: write better "next steps" messages.
// i.e. after setup, run create-db, then run deploy.

export async function setup(baseName: string, region: string, options: IGlobalOptions) {
  echo`Setting up your Wasp app with Fly.io!`

  const buildWaspIfMissing = lazyInit(async () => {
    if (!buildDirExists(options.waspDir)) {
      cd(options.waspDir)
      await $`wasp build`
    }
  })

  const tomlFiles = tomlHelpers.getTomlFileInfo(options)
  const deploymentInfo = new DeploymentInfo(baseName, region, options, tomlFiles)

  if (tomlHelpers.serverTomlExistsInProject(tomlFiles)) {
    echo`${tomlFiles.serverTomlPath} exists. Skipping server setup.`
  } else {
    await buildWaspIfMissing()
    await setupServer(deploymentInfo)
  }

  if (tomlHelpers.clientTomlExistsInProject(tomlFiles)) {
    echo`${tomlFiles.clientTomlPath} exists. Skipping client setup.`
  } else {
    await buildWaspIfMissing()
    await setupClient(deploymentInfo)
  }

  echo`Don't forget to create your database by running the "create-db" command.`
}

async function setupServer(deploymentInfo: IDeploymentInfo) {
  echo`Setting up server app with name ${deploymentInfo.serverName()}`

  cdToServerBuildDir(deploymentInfo.options.waspDir)
  tomlHelpers.deleteLocalToml()

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy --name ${deploymentInfo.serverName()} --region ${deploymentInfo.region}`

  tomlHelpers.copyLocalServerTomlToProject(deploymentInfo.tomlFiles)

  const randomString = crypto.randomBytes(32).toString('hex')
  await $`flyctl secrets set JWT_SECRET=${randomString} PORT=8080 WASP_WEB_CLIENT_URL=${deploymentInfo.clientUrl()}`

  echo`Server setup complete!`
}

async function setupClient(deploymentInfo: IDeploymentInfo) {
  echo`Setting up client app with name ${deploymentInfo.clientName()}`

  cdToClientBuildDir(deploymentInfo.options.waspDir)
  tomlHelpers.deleteLocalToml()

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy --name ${deploymentInfo.clientName()} --region ${deploymentInfo.region}`

  // goStatic listens on port 8043 by default, but the default fly.toml assumes port 8080.
  tomlHelpers.replaceLineInLocalToml(/internal_port = 8080/g, 'internal_port = 8043')

  tomlHelpers.copyLocalClientTomlToProject(deploymentInfo.tomlFiles)

  echo`Client setup complete!`
}
