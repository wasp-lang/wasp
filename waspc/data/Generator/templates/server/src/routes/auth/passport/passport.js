{{={= =}=}}
import express from 'express'
import { initRouter } from './generic/provider.js'

const providerMap = new Map();
{=# providers =}
{=# isEnabled =}
providerMap.set('{= slug =}', { npmPackage: '{= npmPackage =}', passportImportPath: '{= passportImportPath =}' })
{=/ isEnabled =}
{=/ providers =}

const router = express.Router()

async function initProviders(providers) {
  for (let [providerSlug, { npmPackage, passportImportPath }] of providers) {
    const { config, getUserFieldsFn } = await import(passportImportPath)
    const ProviderStrategy = await import(npmPackage)
    router.use(`/${providerSlug}`, initRouter(providerSlug, ProviderStrategy.default, config, getUserFieldsFn))
  }
}

await initProviders(providerMap)

export default router
