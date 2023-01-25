import { $, cd } from 'zx'
import crypto from 'crypto'
import * as tomlHelpers from '../helpers/tomlFileHelpers.js'
import { createDeploymentInfo, DeploymentInfo } from '../DeploymentInfo.js'
import { GlobalOptions } from '../GlobalOptions.js'
import { cdToClientBuildDir, cdToServerBuildDir, lazyInit, getCommandHelp, waspSays } from '../helpers/helpers.js'
import { createFlyDbCommand } from '../index.js'

export async function setup(baseName: string, region: string, options: GlobalOptions) {
  waspSays('Setting up your Wasp app with Fly.io!')

  const buildWasp = lazyInit(async () => {
    waspSays('Building your Wasp app...')
    cd(options.waspDir)
    await $`${options.waspExe} build`
  })

  const tomlFiles = tomlHelpers.getTomlFileInfo(options)
  const deploymentInfo = createDeploymentInfo(baseName, region, options, tomlFiles)

  if (tomlHelpers.serverTomlExistsInProject(tomlFiles)) {
    waspSays(`${tomlFiles.serverTomlPath} exists. Skipping server setup.`)
  } else {
    await buildWasp()
    await setupServer(deploymentInfo)
  }

  if (tomlHelpers.clientTomlExistsInProject(tomlFiles)) {
    waspSays(`${tomlFiles.clientTomlPath} exists. Skipping client setup.`)
  } else {
    await buildWasp()
    await setupClient(deploymentInfo)
  }

  waspSays(`Don't forget to create your database by running "${getCommandHelp(createFlyDbCommand)}".`)
}

async function setupServer(deploymentInfo: DeploymentInfo) {
  waspSays(`Setting up server app with name ${deploymentInfo.serverName}`)

  cdToServerBuildDir(deploymentInfo.options.waspDir)
  tomlHelpers.deleteLocalToml()

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy --name ${deploymentInfo.serverName} --region ${deploymentInfo.region}`

  tomlHelpers.copyLocalServerTomlToProject(deploymentInfo.tomlFiles)

  const randomString = crypto.randomBytes(32).toString('hex')
  await $`flyctl secrets set JWT_SECRET=${randomString} PORT=8080 WASP_WEB_CLIENT_URL=${deploymentInfo.clientUrl}`

  console.log('') // `flyctl secrets` does not produce it's own newline.
  waspSays('Server setup complete!')
}

async function setupClient(deploymentInfo: DeploymentInfo) {
  waspSays(`Setting up client app with name ${deploymentInfo.clientName}`)

  cdToClientBuildDir(deploymentInfo.options.waspDir)
  tomlHelpers.deleteLocalToml()

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy --name ${deploymentInfo.clientName} --region ${deploymentInfo.region}`

  // goStatic listens on port 8043 by default, but the default fly.toml assumes port 8080.
  tomlHelpers.replaceLineInLocalToml(/internal_port = 8080/g, 'internal_port = 8043')

  tomlHelpers.copyLocalClientTomlToProject(deploymentInfo.tomlFiles)

  waspSays('Client setup complete!')
}
