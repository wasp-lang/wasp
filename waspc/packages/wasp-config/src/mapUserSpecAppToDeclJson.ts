import { GET_USER_SPEC } from './_private.js'
import { mapUserSpecToAppSpecDecls } from './mapUserSpecToAppSpecDecls.js'
import * as UserSpec from './userApi.js'

export function mapUserSpecAppToDeclJson(
  app: UserSpec.App,
  entityNames: string[]
): string {
  const userSpec = app[GET_USER_SPEC]()
  const appSpecDecls = mapUserSpecToAppSpecDecls(userSpec, entityNames)

  return JSON.stringify(appSpecDecls)
}
