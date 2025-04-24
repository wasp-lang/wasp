import { GET_USER_SPEC } from './_private.js'
import * as AppSpec from './appSpec.js'
import { mapUserSpecToAppSpecDecls } from './mapUserSpecToAppSpecDecls.js'
import * as UserSpec from './userApi.js'

export function mapUserSpecToAppSpecJson(
  app: UserSpec.App,
  entityNames: string[]
): string {
  const userSpec = getUserSpec(app)
  const appSpecDecls = mapUserSpecToAppSpecDecls(userSpec, entityNames)

  return getDeclsJson(appSpecDecls)
}

export function getUserSpec(app: UserSpec.App): UserSpec.UserSpec {
  return app[GET_USER_SPEC]()
}

function getDeclsJson(appConfig: AppSpec.Decl[]): string {
  return JSON.stringify(appConfig)
}
