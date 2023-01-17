import { exit } from 'process'
import { $, cd, echo, fs } from 'zx'
import { cdToClientBuildDir, cdToServerBuildDir, lazyInit } from '../helpers/helpers.js'
import * as tomlHelpers from '../helpers/tomlFileHelpers.js'
import { IDeployOptions } from './IDeployOptions.js'
import { IDeploymentInfo, DeploymentInfo } from '../DeploymentInfo.js'

export async function deploy(options: IDeployOptions) {
  echo`Deploying your Wasp app to Fly.io!`

  const buildWasp = lazyInit(async () => {
    if (!options.skipBuild) {
      cd(options.waspDir)
      await $`wasp build`
    }
  })

  const tomlFiles = tomlHelpers.getTomlFileInfo(options)

  // NOTE: Below, it would be nice if we could store the client, server, and DB names somewhere.
  // For now we just rely on the suffix naming convention and infer from toml files.
  if (!tomlHelpers.serverTomlExistsInProject(tomlFiles)) {
    echo`${tomlFiles.serverTomlPath} missing. Skipping server deploy. Perhaps you need to run the "setup" command first?`
  } else {
    const inferredBaseName = tomlHelpers.getInferredBasenameFromServerToml(tomlFiles)
    const deploymentInfo = new DeploymentInfo(inferredBaseName, undefined, options, tomlFiles)
    await buildWasp()
    await deployServer(deploymentInfo)
  }

  if (!tomlHelpers.clientTomlExistsInProject(tomlFiles)) {
    echo`${tomlFiles.clientTomlPath} missing. Skipping client deploy. Perhaps you need to run the "setup" command first?`
  } else {
    const inferredBaseName = tomlHelpers.getInferredBasenameFromClientToml(tomlFiles)
    const deploymentInfo = new DeploymentInfo(inferredBaseName, undefined, options, tomlFiles)
    await buildWasp()
    await deployClient(deploymentInfo)
  }
}

async function deployServer(deploymentInfo: IDeploymentInfo) {
  echo`Deploying your server now...`

  cdToServerBuildDir(deploymentInfo.options.waspDir)
  tomlHelpers.copyProjectServerTomlLocally(deploymentInfo.tomlFiles)

  // Make sure we have a DATABASE_URL present. If not, they need to create/attach their DB first.
  try {
    const proc = await $`flyctl secrets list -j`
    const secrets = JSON.parse(proc.stdout)
    if (!secrets.find((s: { Name: string, Digest: string, CreatedAt: string }) => s.Name === 'DATABASE_URL')) {
      echo`Your server app does not have a DATABASE_URL secret set. Perhaps you need to create or attach your database?`
      exit(1)
    }
  } catch {
    echo`Unable to check for DATABASE_URL secret.`
    exit(1)
  }

  await $`flyctl deploy --remote-only`

  tomlHelpers.copyLocalServerTomlToProject(deploymentInfo.tomlFiles)

  echo`Server has been deployed!`
}

async function deployClient(deploymentInfo: IDeploymentInfo) {
  echo`Deploying your client now...`

  cdToClientBuildDir(deploymentInfo.options.waspDir)
  tomlHelpers.copyProjectClientTomlLocally(deploymentInfo.tomlFiles)

  echo`Building web client for production...`
  await $`npm install`
  await $`REACT_APP_API_URL=${deploymentInfo.serverUrl()} npm run build`

  // Creates the necessary Dockerfile for deploying static websites to Fly.io.
  // Adds dummy .dockerignore to supress CLI question.
  // Ref: https://fly.io/docs/languages-and-frameworks/static/
  const dockerfileContents = `
    FROM pierrezemb/gostatic
    CMD [ "-fallback", "index.html" ]
    COPY ./build/ /srv/http/
  `
  fs.writeFileSync('Dockerfile', dockerfileContents)
  fs.writeFileSync('.dockerignore', '')

  await $`flyctl deploy --remote-only`

  tomlHelpers.copyLocalClientTomlToProject(deploymentInfo.tomlFiles)

  echo`Client has been deployed! Your Wasp app is accessible at: ${deploymentInfo.clientUrl()}`
}
