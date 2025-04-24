import { GET_USER_SPEC } from './_private.js'
import { mapUserSpecToAppSpecDecls } from './mapUserSpecToAppSpecDecls.js'
import * as UserSpec from './userApi.js'

export async function tryAnalyzeUserApp(
  mainWaspJs: string,
  entityNames: string[]
): Promise<Result<string, string>> {
  const appResult = await tryGetUserSpec(mainWaspJs)

  if (appResult.status === 'error') {
    return appResult
  }

  const app = appResult.value
  const appSpecDecls = mapUserSpecAppToAppSpecDecls(app, entityNames)

  return {
    status: 'ok',
    value: appSpecDecls,
  }
}

async function tryGetUserSpec(
  mainWaspJs: string
): Promise<Result<UserSpec.App, string>> {
  const usersDefaultExport: unknown = (await import(mainWaspJs)).default
  const appResult = validateUserSpecApp(usersDefaultExport)

  return appResult
}

function validateUserSpecApp(app: unknown): Result<UserSpec.App, string> {
  if (!app) {
    return {
      status: 'error',
      error:
        'Could not load your app config. ' +
        'Make sure your *.wasp.ts file includes a default export of the app.',
    }
  }

  if (!(app instanceof UserSpec.App)) {
    return {
      status: 'error',
      error:
        'The default export of your *.wasp.ts file must be an instance of App. ' +
        'Make sure you export an object created with new App(...).',
    }
  }

  return { status: 'ok', value: app }
}

function mapUserSpecAppToAppSpecDecls(
  app: UserSpec.App,
  entityNames: string[]
): string {
  const userSpec = app[GET_USER_SPEC]()
  const appSpecDecls = mapUserSpecToAppSpecDecls(userSpec, entityNames)
  const appSpecDeclsJson = JSON.stringify(appSpecDecls)

  return appSpecDeclsJson
}

/**
 * Result type is used instead of exceptions for the normal control flow because:
 * - The error users see with the Result type is nicer (no stack trace).
 * - Exceptions can slip through the type system.
 */
type Result<Value, Error> =
  | { status: 'ok'; value: Value }
  | { status: 'error'; error: Error }
