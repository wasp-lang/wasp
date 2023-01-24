import { exit } from 'process'
import { $, question } from 'zx'
import { silence, isYes, waspSays } from './helpers.js'

export async function flyctlExists(): Promise<boolean> {
  try {
    await $`flyctl version`
    return true
  } catch {
    return false
  }
}

export async function isUserLoggedIn(): Promise<boolean> {
  try {
    await $`flyctl auth whoami`
    return true
  } catch {
    return false
  }
}

export async function ensureUserLoggedIn() {
  const userLoggedIn = await isUserLoggedIn()
  if (!userLoggedIn) {
    const answer = await question('flyctl is not logged into Fly.io. Would you like to log in now? ')
    if (isYes(answer)) {
      try {
        await $`flyctl auth login`
      } catch {
        waspSays('It seems there was a problem logging in. Please run "flyctl auth login" and try again.')
        exit(1)
      }
    } else {
      waspSays('Ok, exiting.')
      exit(1)
    }
  }
}

export async function ensureFlyReady() {
  const doesFlyctlExist = await flyctlExists()
  if (!doesFlyctlExist) {
    waspSays('The Fly.io CLI is not available on this system.')
    waspSays('Please install the flyctl here: https://fly.io/docs/hands-on/install-flyctl')
    exit(1)
  }
  await ensureUserLoggedIn()
}

export async function ensureRegionIsValid(region: string) {
  try {
    const proc = await silence(($hh) => $hh`flyctl platform regions -j`)
    const regions = JSON.parse(proc.stdout)
    if (!doesRegionExist(regions, region)) {
      waspSays(`Invalid region code ${region}. Please specify a valid 3 character region id: https://fly.io/docs/reference/regions`)
      exit(1)
    }
  } catch (e) {
    // Ignore any errors while checking. Commands requiring a valid region will still fail if invalid, just not as nicely.
    waspSays('Unable to validate region before calling flyctl. Error: ')
    console.error(e)
  }
}

function doesRegionExist(regions: { Code: string, Name: string }[], region: string): boolean {
  return !!regions.find((r: { Code: string, Name: string }) => r.Code === region)
}

export function doesSecretExist(secrets: { Name: string, Digest: string, CreatedAt: string }[], secretName: string): boolean {
  return !!secrets.find((s) => s.Name === secretName)
}
