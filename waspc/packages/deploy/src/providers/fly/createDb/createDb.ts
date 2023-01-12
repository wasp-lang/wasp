import { $, echo, question } from 'zx'
import { exit } from 'process'
import { getTomlFileInfo, serverTomlExists, getAppNameFromToml } from '../helpers/tomlFileHelpers.js'
import { DeploymentInfo } from '../DeploymentInfo.js'
import { ICreateDbOptions } from './ICreateDbOptions.js'

export async function createDb(region: string, options: ICreateDbOptions) {
  const tomlFiles = getTomlFileInfo(options)

  if (!serverTomlExists(tomlFiles)) {
    echo`${tomlFiles.serverTomlPath} missing. Skipping server deploy. Perhaps you need to run the "setup" command first?`
    exit(1)
  }

  const serverName = getAppNameFromToml(tomlFiles.serverTomlPath)
  const inferredBaseName = serverName.replace('-server', '')
  const deploymentInfo = new DeploymentInfo(inferredBaseName, region, options, tomlFiles)

  // Creates a DB, waits for it to come up, then links it to the app.
  // The attachment process shares the DATABASE_URL secret.
  await $`flyctl postgres create --name ${deploymentInfo.dbName()} --region ${deploymentInfo.region} --vm-size ${options.vmSize} --initial-cluster-size ${options.initialClusterSize} --volume-size ${options.volumeSize}`
  await $`flyctl postgres attach ${deploymentInfo.dbName()} -a ${deploymentInfo.serverName()}`

  await question('Please take note of your database credentials above. Press any key to continue.')
}
