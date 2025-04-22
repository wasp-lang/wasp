import { describe, expect, test } from 'vitest'
import { GET_USER_SPEC } from '../_private.js'
import * as AppSpec from '../appSpec.js'
import { mapUserSpecToAppSpecDecls } from '../mapUserSpecToAppSpecDecls.js'
import * as User from '../userApi.js'
import {
  ACTION,
  API,
  API_NAMESPACE,
  APP,
  AUTH,
  CLIENT,
  CRUD,
  DB,
  EMAIL,
  JOB,
  PAGE,
  QUERY,
  ROUTE,
  SERVER,
  WEBSOCKET,
} from './testFixtures.js'

describe('mapUserSpecToAppSpecDecls', () => {
  test('correctly transforms a complete app configuration', () => {
    const app = new User.App(APP.NAME, APP.CONFIG)
    app.action(ACTION.NAME, ACTION.CONFIG)
    app.apiNamespace(API_NAMESPACE.NAME, API_NAMESPACE.CONFIG)
    app.api(API.NAME, API.CONFIG)
    app.auth(AUTH.CONFIG)
    app.client(CLIENT.CONFIG)
    app.crud(CRUD.NAME, CRUD.CONFIG)
    app.db(DB.CONFIG)
    app.emailSender(EMAIL.CONFIG)
    app.job(JOB.NAME, JOB.CONFIG)
    app.page(PAGE.LOGIN.NAME, PAGE.LOGIN.CONFIG)
    app.page(PAGE.EMAIL_VERIFICATION.NAME, PAGE.EMAIL_VERIFICATION.CONFIG)
    app.page(PAGE.PASSWORD_RESET.NAME, PAGE.PASSWORD_RESET.CONFIG)
    // not passing PageName for simplicity
    app.route(ROUTE.LOGIN.NAME, ROUTE.LOGIN.CONFIG)
    app.route(ROUTE.EMAIL_VERIFICATION.NAME, ROUTE.EMAIL_VERIFICATION.CONFIG)
    app.route(ROUTE.PASSWORD_RESET.NAME, ROUTE.PASSWORD_RESET.CONFIG)
    app.query(QUERY.NAME, QUERY.CONFIG)
    app.server(SERVER.CONFIG)
    app.webSocket(WEBSOCKET.CONFIG)

    const userSpec = app[GET_USER_SPEC]()
    const entityNames: string[] = [
      AUTH.CONFIG.userEntity,
      'Task', // Common entity used by multiple declarations
    ]
    const result = mapUserSpecToAppSpecDecls(userSpec, entityNames)

    const declTypes = result.map((decl) => decl.declType)
    const declNames = result.map((decl) => decl.declName)

    // AppConfig Mapping
    expect(declTypes).toContain('App')
    expect(declNames).toContain(APP.NAME)
    const appDecl = result.find(
      (decl) => decl.declType === 'App'
    ) as AppSpec.GetDeclForType<'App'>
    expect(appDecl).toBeDefined()
    expect(appDecl.declValue.title).toBe(APP.CONFIG.title)
    expect(appDecl.declValue.wasp.version).toBe(APP.CONFIG.wasp.version)
    expect(appDecl.declValue.head).toBeDefined()
    expect(appDecl.declValue.head).toHaveLength(1)
    expect(appDecl.declValue.head?.[0]).toBe(APP.CONFIG.head?.[0])

    // AuthConfig Mapping
    const auth = appDecl.declValue.auth
    expect(auth).toBeDefined()
    expect(auth?.userEntity).toEqual({
      name: AUTH.CONFIG.userEntity,
      declType: 'Entity',
    })
    // Discord
    expect(auth?.methods.discord).toBeDefined()
    expect(auth?.methods.discord?.configFn).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.discord.configFn.import,
      path: AUTH.CONFIG.methods.discord.configFn.from,
    })
    expect(auth?.methods.discord?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.discord.userSignupFields.import,
      path: AUTH.CONFIG.methods.discord.userSignupFields.from,
    })
    // GitHub
    expect(auth?.methods.gitHub).toBeDefined()
    expect(auth?.methods.gitHub?.configFn).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.gitHub.configFn.import,
      path: AUTH.CONFIG.methods.gitHub.configFn.from,
    })
    expect(auth?.methods.gitHub?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.gitHub.userSignupFields.import,
      path: AUTH.CONFIG.methods.gitHub.userSignupFields.from,
    })
    // Keycloak
    expect(auth?.methods.keycloak).toBeDefined()
    expect(auth?.methods.keycloak?.configFn).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.keycloak.configFn.import,
      path: AUTH.CONFIG.methods.keycloak.configFn.from,
    })
    expect(auth?.methods.keycloak?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.keycloak.userSignupFields.import,
      path: AUTH.CONFIG.methods.keycloak.userSignupFields.from,
    })
    // Google
    expect(auth?.methods.google).toBeDefined()
    expect(auth?.methods.google?.configFn).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.google.configFn.import,
      path: AUTH.CONFIG.methods.google.configFn.from,
    })
    expect(auth?.methods.google?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.google.userSignupFields.import,
      path: AUTH.CONFIG.methods.google.userSignupFields.from,
    })
    // Username and Password
    expect(auth?.methods.usernameAndPassword).toBeDefined()
    expect(auth?.methods.usernameAndPassword?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.usernameAndPassword.userSignupFields.import,
      path: AUTH.CONFIG.methods.usernameAndPassword.userSignupFields.from,
    })
    // Email
    expect(auth?.methods.email).toBeDefined()
    expect(auth?.methods.email?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.email.userSignupFields.import,
      path: AUTH.CONFIG.methods.email.userSignupFields.from,
    })
    expect(auth?.methods.email?.fromField).toEqual({
      name: AUTH.CONFIG.methods.email.fromField.name,
      email: AUTH.CONFIG.methods.email.fromField.email,
    })
    expect(auth?.methods.email?.emailVerification).toEqual({
      getEmailContentFn: {
        kind: 'named',
        name: AUTH.CONFIG.methods.email.emailVerification.getEmailContentFn
          .import,
        path: AUTH.CONFIG.methods.email.emailVerification.getEmailContentFn
          .from,
      },
      clientRoute: {
        name: AUTH.CONFIG.methods.email.emailVerification.clientRoute,
        declType: 'Route',
      },
    })
    expect(auth?.methods.email?.passwordReset).toEqual({
      getEmailContentFn: {
        kind: 'named',
        name: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn.import,
        path: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn.from,
      },
      clientRoute: {
        name: AUTH.CONFIG.methods.email.passwordReset.clientRoute,
        declType: 'Route',
      },
    })
    // Hooks
    expect(auth?.onAuthFailedRedirectTo).toBe(
      AUTH.CONFIG.onAuthFailedRedirectTo
    )
    expect(auth?.onAuthSucceededRedirectTo).toBe(
      AUTH.CONFIG.onAuthSucceededRedirectTo
    )
    expect(auth?.onBeforeOAuthRedirect).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.onBeforeOAuthRedirect.import,
      path: AUTH.CONFIG.onBeforeOAuthRedirect.from,
    })
    expect(auth?.onBeforeSignup).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.onBeforeSignup.import,
      path: AUTH.CONFIG.onBeforeSignup.from,
    })
    expect(auth?.onAfterSignup).toEqual({
      kind: 'named',
      name: AUTH.CONFIG.onAfterSignup.import,
      path: AUTH.CONFIG.onAfterSignup.from,
    })

    // ClientConfig Mapping
    const client = appDecl.declValue.client
    expect(client).toBeDefined()
    expect(client?.rootComponent).toEqual({
      kind: 'named',
      name: CLIENT.CONFIG.rootComponent.import,
      path: CLIENT.CONFIG.rootComponent.from,
    })
    expect(client?.setupFn).toEqual({
      kind: 'named',
      name: CLIENT.CONFIG.setupFn.import,
      path: CLIENT.CONFIG.setupFn.from,
    })

    // DbConfig Mapping
    const db = appDecl.declValue.db
    expect(db).toBeDefined()
    expect(db?.seeds).toHaveLength(1)
    expect(db?.seeds?.[0]).toEqual({
      kind: 'named',
      name: DB.CONFIG.seeds[0]?.import,
      path: DB.CONFIG.seeds[0]?.from,
    })

    // EmailSenderConfig Mapping
    const emailSender = appDecl.declValue.emailSender
    expect(emailSender).toBeDefined()
    expect(emailSender?.provider).toBe(EMAIL.CONFIG.provider)
    expect(emailSender?.defaultFrom?.email).toBe(EMAIL.CONFIG.defaultFrom.email)

    // ServerConfig Mapping
    const server = appDecl.declValue.server
    expect(server).toBeDefined()
    expect(server?.setupFn).toEqual({
      kind: 'named',
      name: SERVER.CONFIG.setupFn.import,
      path: SERVER.CONFIG.setupFn.from,
    })

    // WebSocketConfig Mapping
    const webSocket = appDecl.declValue.webSocket
    expect(webSocket).toBeDefined()
    expect(webSocket?.fn).toEqual({
      kind: 'named',
      name: WEBSOCKET.CONFIG.fn.import,
      path: WEBSOCKET.CONFIG.fn.from,
    })

    // ActionConfig Mapping
    expect(declTypes).toContain('Action')
    expect(declNames).toContain(ACTION.NAME)
    const actionDecl = result.find(
      (decl) => decl.declType === 'Action' && decl.declName === ACTION.NAME
    ) as AppSpec.GetDeclForType<'Action'>
    expect(actionDecl).toBeDefined()
    expect(actionDecl.declValue.fn).toEqual({
      kind: 'named',
      name: ACTION.CONFIG.fn.import,
      path: ACTION.CONFIG.fn.from,
    })
    expect(actionDecl.declValue.entities).toBeDefined()
    expect(actionDecl.declValue.entities).toHaveLength(1)
    expect(actionDecl.declValue.entities?.[0]).toEqual({
      name: ACTION.CONFIG.entities[0],
      declType: 'Entity',
    })

    // ApiNamespaceConfig Mapping
    expect(declTypes).toContain('ApiNamespace')
    expect(declNames).toContain(API_NAMESPACE.NAME)
    const apiNamespaceDecl = result.find(
      (decl) =>
        decl.declType === 'ApiNamespace' && decl.declName === API_NAMESPACE.NAME
    ) as AppSpec.GetDeclForType<'ApiNamespace'>
    expect(apiNamespaceDecl).toBeDefined()
    expect(apiNamespaceDecl.declValue.path).toBe(API_NAMESPACE.CONFIG.path)
    expect(apiNamespaceDecl.declValue.middlewareConfigFn).toEqual({
      kind: 'named',
      name: API_NAMESPACE.CONFIG.middlewareConfigFn.import,
      path: API_NAMESPACE.CONFIG.middlewareConfigFn.from,
    })

    // ApiConfig Mapping
    expect(declTypes).toContain('Api')
    expect(declNames).toContain(API.NAME)
    const apiDecl = result.find(
      (decl) => decl.declType === 'Api' && decl.declName === API.NAME
    ) as AppSpec.GetDeclForType<'Api'>
    expect(apiDecl.declValue.fn).toEqual({
      kind: 'named',
      name: API.CONFIG.fn.import,
      path: API.CONFIG.fn.from,
    })
    expect(apiDecl.declValue.auth).toBe(API.CONFIG.auth)
    expect(apiDecl.declValue.httpRoute).toEqual([
      API.CONFIG.httpRoute.method,
      API.CONFIG.httpRoute.route,
    ])
    expect(apiDecl.declValue.entities).toBeDefined()
    expect(apiDecl.declValue.entities).toHaveLength(1)
    expect(apiDecl.declValue.entities?.[0]).toEqual({
      name: API.CONFIG.entities[0],
      declType: 'Entity',
    })

    // CrudConfig Mapping
    expect(declTypes).toContain('Crud')
    expect(declNames).toContain(CRUD.NAME)
    const crudDecl = result.find(
      (decl) => decl.declType === 'Crud' && decl.declName === CRUD.NAME
    ) as AppSpec.GetDeclForType<'Crud'>
    expect(crudDecl).toBeDefined()
    expect(crudDecl.declValue.entity).toEqual({
      name: CRUD.CONFIG.entity,
      declType: 'Entity',
    })
    expect(crudDecl.declValue.operations.get).toBeDefined()
    expect(crudDecl.declValue.operations.get?.isPublic).toBe(
      CRUD.CONFIG.operations.get.isPublic
    )
    expect(crudDecl.declValue.operations.get?.overrideFn).toEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.get.overrideFn.import,
      path: CRUD.CONFIG.operations.get.overrideFn.from,
    })
    expect(crudDecl.declValue.operations.create).toBeDefined()
    expect(crudDecl.declValue.operations.create?.isPublic).toBe(
      CRUD.CONFIG.operations.create.isPublic
    )
    expect(crudDecl.declValue.operations.update).toBeDefined()
    expect(crudDecl.declValue.operations.update?.isPublic).toBe(
      CRUD.CONFIG.operations.update.isPublic
    )
    expect(crudDecl.declValue.operations.delete).toBeDefined()
    expect(crudDecl.declValue.operations.delete?.isPublic).toBe(
      CRUD.CONFIG.operations.delete.isPublic
    )
    expect(crudDecl.declValue.operations.delete?.overrideFn).toEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.delete.overrideFn.import,
      path: CRUD.CONFIG.operations.delete.overrideFn.from,
    })

    // JobConfig Mapping
    expect(declTypes).toContain('Job')
    expect(declNames).toContain(JOB.NAME)
    const jobDecl = result.find(
      (decl) => decl.declType === 'Job' && decl.declName === JOB.NAME
    ) as AppSpec.GetDeclForType<'Job'>
    expect(jobDecl.declValue.executor).toBe(JOB.CONFIG.executor)
    expect(jobDecl.declValue.perform.fn).toEqual({
      kind: 'named',
      name: JOB.CONFIG.perform.fn.import,
      path: JOB.CONFIG.perform.fn.from,
    })
    expect(jobDecl.declValue.entities?.[0]).toEqual({
      name: JOB.CONFIG.entities[0],
      declType: 'Entity',
    })

    // PageConfig Mapping
    expect(declTypes).toContain('Page')
    expect(declNames).toContain(PAGE.LOGIN.NAME)
    const loginPageDecl = result.find(
      (decl) => decl.declType === 'Page' && decl.declName === PAGE.LOGIN.NAME
    ) as AppSpec.GetDeclForType<'Page'>
    expect(loginPageDecl.declValue.component).toEqual({
      kind: 'named',
      name: PAGE.LOGIN.CONFIG.component.import,
      path: PAGE.LOGIN.CONFIG.component.from,
    })
    expect(declNames).toContain(PAGE.EMAIL_VERIFICATION.NAME)
    const emailVerificationPageDecl = result.find(
      (decl) =>
        decl.declType === 'Page' &&
        decl.declName === PAGE.EMAIL_VERIFICATION.NAME
    ) as AppSpec.GetDeclForType<'Page'>
    expect(emailVerificationPageDecl.declValue.component).toEqual({
      kind: 'named',
      name: PAGE.EMAIL_VERIFICATION.CONFIG.component.import,
      path: PAGE.EMAIL_VERIFICATION.CONFIG.component.from,
    })
    expect(declNames).toContain(PAGE.PASSWORD_RESET.NAME)
    const passwordResetPageDecl = result.find(
      (decl) =>
        decl.declType === 'Page' && decl.declName === PAGE.PASSWORD_RESET.NAME
    ) as AppSpec.GetDeclForType<'Page'>
    expect(passwordResetPageDecl.declValue.component).toEqual({
      kind: 'named',
      name: PAGE.PASSWORD_RESET.CONFIG.component.import,
      path: PAGE.PASSWORD_RESET.CONFIG.component.from,
    })

    // RouteConfig Mapping
    expect(declTypes).toContain('Route')
    expect(declNames).toContain(ROUTE.LOGIN.NAME)
    const routeDecl = result.find(
      (decl) => decl.declType === 'Route' && decl.declName === ROUTE.LOGIN.NAME
    ) as AppSpec.GetDeclForType<'Route'>
    expect(routeDecl.declValue.path).toBe(ROUTE.LOGIN.CONFIG.path)
    expect(routeDecl.declValue.to).toEqual({
      name: PAGE.LOGIN.NAME,
      declType: 'Page',
    })
    expect(declNames).toContain(ROUTE.EMAIL_VERIFICATION.NAME)
    const emailVerificationRouteDecl = result.find(
      (decl) =>
        decl.declType === 'Route' &&
        decl.declName === ROUTE.EMAIL_VERIFICATION.NAME
    ) as AppSpec.GetDeclForType<'Route'>
    expect(emailVerificationRouteDecl.declValue.path).toBe(
      ROUTE.EMAIL_VERIFICATION.CONFIG.path
    )
    expect(emailVerificationRouteDecl.declValue.to).toEqual({
      name: PAGE.EMAIL_VERIFICATION.NAME,
      declType: 'Page',
    })
    expect(declNames).toContain(ROUTE.PASSWORD_RESET.NAME)
    const passwordResetRouteDecl = result.find(
      (decl) =>
        decl.declType === 'Route' && decl.declName === ROUTE.PASSWORD_RESET.NAME
    ) as AppSpec.GetDeclForType<'Route'>
    expect(passwordResetRouteDecl.declValue.path).toBe(
      ROUTE.PASSWORD_RESET.CONFIG.path
    )
    expect(passwordResetRouteDecl.declValue.to).toEqual({
      name: PAGE.PASSWORD_RESET.NAME,
      declType: 'Page',
    })

    // QueryConfig Mapping
    expect(declTypes).toContain('Query')
    expect(declNames).toContain(QUERY.NAME)
    const queryDecl = result.find(
      (decl) => decl.declType === 'Query' && decl.declName === QUERY.NAME
    ) as AppSpec.GetDeclForType<'Query'>
    expect(queryDecl.declValue.fn).toEqual({
      kind: 'named',
      name: QUERY.CONFIG.fn.import,
      path: QUERY.CONFIG.fn.from,
    })
    expect(queryDecl.declValue.entities?.[0]).toEqual({
      name: QUERY.CONFIG.entities[0],
      declType: 'Entity',
    })
  })
})
