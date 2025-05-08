import { GET_USER_SPEC } from './_private.js'
import { mapUserSpecToAppSpecDecls } from './mapUserSpecToAppSpecDecls.js'
import * as UserApi from './userApi.js'

export async function analyzeUserApp(
  waspTsSpecPath: string,
  entityNames: string[]
): Promise<Result<string, string>> {
  const userAppResult = await getUserApp(waspTsSpecPath)

  if (userAppResult.status === 'error') {
    return userAppResult
  }

  const userApp = userAppResult.value
  const userSpec = userApp[GET_USER_SPEC]()
  const appSpecDecls = mapUserSpecToAppSpecDecls(userSpec, entityNames)

  return {
    status: 'ok',
    value: JSON.stringify(appSpecDecls),
  }
}

export async function getUserApp(
  mainWaspJs: string
): Promise<Result<UserApi.App, string>> {
  const usersDefaultExport: unknown = (await import(mainWaspJs)).default

  if (!usersDefaultExport) {
    return {
      status: 'error',
      error:
        'Could not load your app config. ' +
        'Make sure your *.wasp.ts file includes a default export of the app.',
    }
  }

  if (!(usersDefaultExport instanceof UserApi.App)) {
    return {
      status: 'error',
      error:
        'The default export of your *.wasp.ts file must be an instance of App. ' +
        'Make sure you export an object created with new App(...).',
    }
  }

  return { status: 'ok', value: usersDefaultExport }
}

/**
 * Result type is used instead of exceptions for the normal control flow because:
 * - The error users see with the Result type is nicer (no stack trace).
 * - Exceptions can slip through the type system.
 */
export type Result<Value, Error> =
  | { status: 'ok'; value: Value }
  | { status: 'error'; error: Error }
