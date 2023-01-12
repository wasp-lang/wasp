import { $, echo } from 'zx'
import crypto from 'crypto'
import * as tomlHelpers from '../helpers/tomlFileHelpers.js'
import { DeploymentInfo, IDeploymentInfo } from '../DeploymentInfo.js'
import { IGlobalOptions } from '../IGlobalOptions.js'
import { cdToClientDir, cdToServerDir } from '../helpers/helpers.js'

export async function setup(baseName: string, region: string, options: IGlobalOptions) {
  echo`Setting up your Wasp app with Fly.io!`

  const tomlFiles = tomlHelpers.getTomlFileInfo(options)
  const deploymentInfo = new DeploymentInfo(baseName, region, options, tomlFiles)

  if (tomlHelpers.serverTomlExists(tomlFiles)) {
    echo`${tomlFiles.serverTomlPath} exists. Skipping server setup.`
  } else {
    await setupServer(deploymentInfo)
  }

  if (tomlHelpers.clientTomlExists(tomlFiles)) {
    echo`${tomlFiles.clientTomlPath} exists. Skipping client setup.`
  } else {
    await setupClient(deploymentInfo)
  }
}

async function setupServer(deploymentInfo: IDeploymentInfo) {
  echo`Setting up server app with name ${deploymentInfo.serverName()}`

  cdToServerDir(deploymentInfo.options.waspDir)
  tomlHelpers.deleteLocalToml()

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy --name ${deploymentInfo.serverName()} --region ${deploymentInfo.region}`

  tomlHelpers.copyLocalTomlAsServerToml(deploymentInfo.tomlFiles)

  const randomString = crypto.randomBytes(32).toString('hex')
  await $`flyctl secrets set JWT_SECRET=${randomString} PORT=8080 WASP_WEB_CLIENT_URL=${deploymentInfo.clientUrl()}`

  echo`Server setup complete!`
  echo`Don't forget to create your database by running the "create-db" command.`
}

async function setupClient(deploymentInfo: IDeploymentInfo) {
  echo`Setting up client app with name ${deploymentInfo.clientName()}`

  cdToClientDir(deploymentInfo.options.waspDir)
  tomlHelpers.deleteLocalToml()

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy --name ${deploymentInfo.clientName()} --region ${deploymentInfo.region}`

  // goStatic listens on port 8043 by default, but the default fly.toml assumes port 8080.
  tomlHelpers.replaceLineInLocalToml(/internal_port = 8080/g, 'internal_port = 8043')

  tomlHelpers.copyLocalTomlAsClientToml(deploymentInfo.tomlFiles)

  echo`Client setup complete!`
}
