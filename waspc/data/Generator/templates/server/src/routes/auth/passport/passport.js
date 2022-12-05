{{={= =}=}}
import express from 'express'
import { initRouter } from './generic/provider.js'

const providerMap = new Map();
{=# providers =}
{=# isEnabled =}
providerMap.set('{= name =}', '{= npmPackage =}')
{=/ isEnabled =}
{=/ providers =}

const router = express.Router()

async function initProviders(providers) {
  for (let [providerName, npmPackageName] of providers) {
    const { config, getUserFieldsFn } = await import(`./${providerName}/${providerName}.js`)
    const ProviderStrategy = await import(npmPackageName)
    router.use(`/${providerName}`, initRouter(providerName, ProviderStrategy.default, config, getUserFieldsFn))
  }
}

await initProviders(providerMap)

export default router
