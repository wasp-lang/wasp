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
    const app = new User.App(APP.NAME, {
      title: APP.TITLE,
      wasp: { version: APP.VERSION },
      head: [APP.HEAD_FAVICON],
    })

    app.action(ACTION.NAME, {
      fn: { import: ACTION.NAME, from: ACTION.FROM },
      entities: [ACTION.ENTITY],
    })

    app.apiNamespace(API_NAMESPACE.NAME, {
      middlewareConfigFn: {
        import: API_NAMESPACE.MIDDLEWARE_CONFIG_FN.IMPORT,
        from: API_NAMESPACE.MIDDLEWARE_CONFIG_FN.FROM,
      },
      path: API_NAMESPACE.PATH,
    })

    app.api(API.NAME, {
      fn: { import: API.FN.IMPORT, from: API.FN.FROM },
      auth: API.AUTH,
      entities: [API.ENTITY],
      httpRoute: {
        method: API.HTTP_ROUTE.METHOD,
        route: API.HTTP_ROUTE.ROUTE,
      },
    })

    app.auth({
      userEntity: AUTH.ENTITY,
      methods: {
        discord: {
          configFn: {
            import: AUTH.METHODS.DISCORD.CONFIG_FN.IMPORT,
            from: AUTH.METHODS.DISCORD.CONFIG_FN.FROM,
          },
          userSignupFields: {
            import: AUTH.METHODS.DISCORD.USER_SIGNUP_FIELDS.IMPORT,
            from: AUTH.METHODS.DISCORD.USER_SIGNUP_FIELDS.FROM,
          },
        },
        google: {
          configFn: {
            import: AUTH.METHODS.GOOGLE.CONFIG_FN.IMPORT,
            from: AUTH.METHODS.GOOGLE.CONFIG_FN.FROM,
          },
          userSignupFields: {
            import: AUTH.METHODS.GOOGLE.USER_SIGNUP_FIELDS.IMPORT,
            from: AUTH.METHODS.GOOGLE.USER_SIGNUP_FIELDS.FROM,
          },
        },
        gitHub: {
          configFn: {
            import: AUTH.METHODS.GITHUB.CONFIG_FN.IMPORT,
            from: AUTH.METHODS.GITHUB.CONFIG_FN.FROM,
          },
          userSignupFields: {
            import: AUTH.METHODS.GITHUB.USER_SIGNUP_FIELDS.IMPORT,
            from: AUTH.METHODS.GITHUB.USER_SIGNUP_FIELDS.FROM,
          },
        },
        keycloak: {
          configFn: {
            import: AUTH.METHODS.KEYCLOAK.CONFIG_FN.IMPORT,
            from: AUTH.METHODS.KEYCLOAK.CONFIG_FN.FROM,
          },
          userSignupFields: {
            import: AUTH.METHODS.KEYCLOAK.USER_SIGNUP_FIELDS.IMPORT,
            from: AUTH.METHODS.KEYCLOAK.USER_SIGNUP_FIELDS.FROM,
          },
        },
        usernameAndPassword: {
          userSignupFields: {
            import:
              AUTH.METHODS.USERNAME_AND_PASSWORD.USER_SIGNUP_FIELDS.IMPORT,
            from: AUTH.METHODS.USERNAME_AND_PASSWORD.USER_SIGNUP_FIELDS.FROM,
          },
        },
        email: {
          userSignupFields: {
            import: AUTH.METHODS.EMAIL.USER_SIGNUP_FIELDS.IMPORT,
            from: AUTH.METHODS.EMAIL.USER_SIGNUP_FIELDS.FROM,
          },
          fromField: {
            name: AUTH.METHODS.EMAIL.FROM_FIELD.NAME,
            email: AUTH.METHODS.EMAIL.FROM_FIELD.EMAIL,
          },
          emailVerification: {
            getEmailContentFn: {
              import:
                AUTH.METHODS.EMAIL.EMAIL_VERIFICATION.GET_EMAIL_CONTENT_FN
                  .IMPORT,
              from: AUTH.METHODS.EMAIL.EMAIL_VERIFICATION.GET_EMAIL_CONTENT_FN
                .FROM,
            },
            clientRoute: AUTH.METHODS.EMAIL.EMAIL_VERIFICATION.CLIENT_ROUTE,
          },
          passwordReset: {
            getEmailContentFn: {
              import:
                AUTH.METHODS.EMAIL.PASSWORD_RESET.GET_EMAIL_CONTENT_FN.IMPORT,
              from: AUTH.METHODS.EMAIL.PASSWORD_RESET.GET_EMAIL_CONTENT_FN.FROM,
            },
            clientRoute: AUTH.METHODS.EMAIL.PASSWORD_RESET.CLIENT_ROUTE,
          },
        },
      },
      onAuthFailedRedirectTo: AUTH.ON_AUTH_FAILED_REDIRECT_TO,
      onAuthSucceededRedirectTo: AUTH.ON_AUTH_SUCCEEDED_REDIRECT_TO,
      onBeforeSignup: {
        import: AUTH.ON_BEFORE_SIGNUP.IMPORT,
        from: AUTH.ON_BEFORE_SIGNUP.FROM,
      },
      onAfterSignup: {
        import: AUTH.ON_AFTER_SIGNUP.IMPORT,
        from: AUTH.ON_AFTER_SIGNUP.FROM,
      },
      onBeforeOAuthRedirect: {
        import: AUTH.ON_BEFORE_OAUTH_REDIRECT.IMPORT,
        from: AUTH.ON_BEFORE_OAUTH_REDIRECT.FROM,
      },
      onBeforeLogin: {
        import: AUTH.ON_BEFORE_LOGIN.IMPORT,
        from: AUTH.ON_BEFORE_LOGIN.FROM,
      },
      onAfterLogin: {
        import: AUTH.ON_AFTER_LOGIN.IMPORT,
        from: AUTH.ON_AFTER_LOGIN.FROM,
      },
    })

    app.client({
      rootComponent: {
        import: CLIENT.ROOT_COMPONENT.IMPORT,
        from: CLIENT.ROOT_COMPONENT.FROM,
      },
      setupFn: {
        import: CLIENT.SETUP_FN.IMPORT,
        from: CLIENT.SETUP_FN.FROM,
      },
    })

    app.crud(CRUD.NAME, {
      entity: CRUD.ENTITY,
      operations: {
        get: {
          isPublic: CRUD.OPERATIONS.GET.IS_PUBLIC,
          overrideFn: {
            import: CRUD.OPERATIONS.GET.OVERRIDE_FN.IMPORT,
            from: CRUD.OPERATIONS.GET.OVERRIDE_FN.FROM,
          },
        },
        getAll: { isPublic: CRUD.OPERATIONS.GET_ALL.IS_PUBLIC },
        create: { isPublic: CRUD.OPERATIONS.CREATE.IS_PUBLIC },
        update: { isPublic: CRUD.OPERATIONS.UPDATE.IS_PUBLIC },
        delete: {
          isPublic: CRUD.OPERATIONS.DELETE.IS_PUBLIC,
          overrideFn: {
            import: CRUD.OPERATIONS.DELETE.OVERRIDE_FN.IMPORT,
            from: CRUD.OPERATIONS.DELETE.OVERRIDE_FN.FROM,
          },
        },
      },
    })

    app.db({
      seeds: [{ import: DB.SEEDS.IMPORT, from: DB.SEEDS.FROM }],
    })

    app.emailSender({
      provider: EMAIL.SMTP.PROVIDER,
      defaultFrom: { email: EMAIL.SMTP.ADDRESS },
    })

    app.job(JOB.NAME, {
      executor: JOB.EXECUTOR,
      perform: {
        fn: {
          import: JOB.PERFORM.FN.IMPORT,
          from: JOB.PERFORM.FN.FROM,
        },
        // TODO: add pgboss once its better typed
      },
      entities: [JOB.ENTITY],
    })

    const loginPage = app.page(PAGE.LOGIN.NAME, {
      component: {
        import: PAGE.LOGIN.COMPONENT.IMPORT,
        from: PAGE.LOGIN.COMPONENT.FROM,
      },
    })
    app.route(ROUTE.LOGIN.NAME, { path: ROUTE.LOGIN.PATH, to: loginPage })

    const emailVerificationPage = app.page(PAGE.EMAIL_VERIFICATION.NAME, {
      component: {
        import: PAGE.EMAIL_VERIFICATION.COMPONENT.IMPORT,
        from: PAGE.EMAIL_VERIFICATION.COMPONENT.FROM,
      },
    })
    app.route(ROUTE.EMAIL_VERIFICATION.NAME, {
      path: ROUTE.EMAIL_VERIFICATION.PATH,
      to: emailVerificationPage,
    })

    const passwordResetPage = app.page(PAGE.PASSWORD_RESET.NAME, {
      component: {
        import: PAGE.PASSWORD_RESET.COMPONENT.IMPORT,
        from: PAGE.PASSWORD_RESET.COMPONENT.FROM,
      },
    })
    app.route(ROUTE.PASSWORD_RESET.NAME, {
      path: ROUTE.PASSWORD_RESET.PATH,
      to: passwordResetPage,
    })

    app.query(QUERY.NAME, {
      fn: { import: QUERY.NAME, from: QUERY.FROM },
      entities: [QUERY.ENTITY],
    })

    app.server({
      setupFn: {
        import: SERVER.SETUP_IMPORT_DEFAULT,
        from: SERVER.SETUP_FROM,
      },
      middlewareConfigFn: {
        import: SERVER.MIDDLEWARE_IMPORT,
        from: SERVER.SETUP_FROM,
      },
    })
    app.webSocket({
      fn: { import: WEBSOCKET.FN_IMPORT, from: WEBSOCKET.FROM },
      autoConnect: WEBSOCKET.AUTO_CONNECT,
    })

    const userSpec = app[GET_USER_SPEC]()
    const entityNames: string[] = [
      AUTH.ENTITY,
      ACTION.ENTITY,
      QUERY.ENTITY,
      API.ENTITY,
      JOB.ENTITY,
      CRUD.ENTITY,
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
    expect(appDecl.declValue.title).toBe(APP.TITLE)
    expect(appDecl.declValue.wasp.version).toBe(APP.VERSION)
    expect(appDecl.declValue.head).toBeDefined()
    expect(appDecl.declValue.head).toHaveLength(1)
    expect(appDecl.declValue.head?.[0]).toBe(APP.HEAD_FAVICON)

    // AuthConfig Mapping
    const auth = appDecl.declValue.auth
    expect(auth).toBeDefined()
    expect(auth?.userEntity).toEqual({
      name: AUTH.ENTITY,
      declType: 'Entity',
    })
    // Discrd
    expect(auth?.methods.discord).toBeDefined()
    expect(auth?.methods.discord?.configFn).toEqual({
      kind: 'named',
      name: AUTH.METHODS.DISCORD.CONFIG_FN.IMPORT,
      path: AUTH.METHODS.DISCORD.CONFIG_FN.FROM,
    })
    expect(auth?.methods.discord?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.METHODS.DISCORD.USER_SIGNUP_FIELDS.IMPORT,
      path: AUTH.METHODS.DISCORD.USER_SIGNUP_FIELDS.FROM,
    })
    // GitHub
    expect(auth?.methods.gitHub).toBeDefined()
    expect(auth?.methods.gitHub?.configFn).toEqual({
      kind: 'named',
      name: AUTH.METHODS.GITHUB.CONFIG_FN.IMPORT,
      path: AUTH.METHODS.GITHUB.CONFIG_FN.FROM,
    })
    expect(auth?.methods.gitHub?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.METHODS.GITHUB.USER_SIGNUP_FIELDS.IMPORT,
      path: AUTH.METHODS.GITHUB.USER_SIGNUP_FIELDS.FROM,
    })
    // Keycloak
    expect(auth?.methods.keycloak).toBeDefined()
    expect(auth?.methods.keycloak?.configFn).toEqual({
      kind: 'named',
      name: AUTH.METHODS.KEYCLOAK.CONFIG_FN.IMPORT,
      path: AUTH.METHODS.KEYCLOAK.CONFIG_FN.FROM,
    })
    expect(auth?.methods.keycloak?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.METHODS.KEYCLOAK.USER_SIGNUP_FIELDS.IMPORT,
      path: AUTH.METHODS.KEYCLOAK.USER_SIGNUP_FIELDS.FROM,
    })
    // Google
    expect(auth?.methods.google).toBeDefined()
    expect(auth?.methods.google?.configFn).toEqual({
      kind: 'named',
      name: AUTH.METHODS.GOOGLE.CONFIG_FN.IMPORT,
      path: AUTH.METHODS.GOOGLE.CONFIG_FN.FROM,
    })
    expect(auth?.methods.google?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.METHODS.GOOGLE.USER_SIGNUP_FIELDS.IMPORT,
      path: AUTH.METHODS.GOOGLE.USER_SIGNUP_FIELDS.FROM,
    })
    // Username and Password
    expect(auth?.methods.usernameAndPassword).toBeDefined()
    expect(auth?.methods.usernameAndPassword?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.METHODS.USERNAME_AND_PASSWORD.USER_SIGNUP_FIELDS.IMPORT,
      path: AUTH.METHODS.USERNAME_AND_PASSWORD.USER_SIGNUP_FIELDS.FROM,
    })
    // Email
    expect(auth?.methods.email).toBeDefined()
    expect(auth?.methods.email?.userSignupFields).toEqual({
      kind: 'named',
      name: AUTH.METHODS.EMAIL.USER_SIGNUP_FIELDS.IMPORT,
      path: AUTH.METHODS.EMAIL.USER_SIGNUP_FIELDS.FROM,
    })
    expect(auth?.methods.email?.fromField).toEqual({
      name: AUTH.METHODS.EMAIL.FROM_FIELD.NAME,
      email: AUTH.METHODS.EMAIL.FROM_FIELD.EMAIL,
    })
    expect(auth?.methods.email?.emailVerification).toEqual({
      getEmailContentFn: {
        kind: 'named',
        name: AUTH.METHODS.EMAIL.EMAIL_VERIFICATION.GET_EMAIL_CONTENT_FN.IMPORT,
        path: AUTH.METHODS.EMAIL.EMAIL_VERIFICATION.GET_EMAIL_CONTENT_FN.FROM,
      },
      clientRoute: {
        name: AUTH.METHODS.EMAIL.EMAIL_VERIFICATION.CLIENT_ROUTE,
        declType: 'Route',
      },
    })
    expect(auth?.methods.email?.passwordReset).toEqual({
      getEmailContentFn: {
        kind: 'named',
        name: AUTH.METHODS.EMAIL.PASSWORD_RESET.GET_EMAIL_CONTENT_FN.IMPORT,
        path: AUTH.METHODS.EMAIL.PASSWORD_RESET.GET_EMAIL_CONTENT_FN.FROM,
      },
      clientRoute: {
        name: AUTH.METHODS.EMAIL.PASSWORD_RESET.CLIENT_ROUTE,
        declType: 'Route',
      },
    })
    // Hooks
    expect(auth?.onAuthFailedRedirectTo).toBe(AUTH.ON_AUTH_FAILED_REDIRECT_TO)
    expect(auth?.onAuthSucceededRedirectTo).toBe(
      AUTH.ON_AUTH_SUCCEEDED_REDIRECT_TO
    )
    expect(auth?.onBeforeOAuthRedirect).toEqual({
      kind: 'named',
      name: AUTH.ON_BEFORE_OAUTH_REDIRECT.IMPORT,
      path: AUTH.ON_BEFORE_OAUTH_REDIRECT.FROM,
    })
    expect(auth?.onBeforeSignup).toEqual({
      kind: 'named',
      name: AUTH.ON_BEFORE_SIGNUP.IMPORT,
      path: AUTH.ON_BEFORE_SIGNUP.FROM,
    })
    expect(auth?.onAfterSignup).toEqual({
      kind: 'named',
      name: AUTH.ON_AFTER_SIGNUP.IMPORT,
      path: AUTH.ON_AFTER_SIGNUP.FROM,
    })

    // ClientConfig Mapping
    const client = appDecl.declValue.client
    expect(client).toBeDefined()
    expect(client?.rootComponent).toEqual({
      kind: 'named',
      name: CLIENT.ROOT_COMPONENT.IMPORT,
      path: CLIENT.ROOT_COMPONENT.FROM,
    })
    expect(client?.setupFn).toEqual({
      kind: 'named',
      name: CLIENT.SETUP_FN.IMPORT,
      path: CLIENT.SETUP_FN.FROM,
    })

    // DbConfig Mapping
    const db = appDecl.declValue.db
    expect(db).toBeDefined()
    expect(db?.seeds).toHaveLength(1)
    expect(db?.seeds?.[0]).toEqual({
      kind: 'named',
      name: DB.SEEDS.IMPORT,
      path: DB.SEEDS.FROM,
    })

    // EmailSenderConfig Mapping
    const emailSender = appDecl.declValue.emailSender
    expect(emailSender).toBeDefined()
    expect(emailSender?.provider).toBe(EMAIL.SMTP.PROVIDER)
    expect(emailSender?.defaultFrom?.email).toBe(EMAIL.SMTP.ADDRESS)

    // ServerConfig Mapping
    const server = appDecl.declValue.server
    expect(server).toBeDefined()
    expect(server?.setupFn).toEqual({
      kind: 'named',
      name: SERVER.SETUP_IMPORT_DEFAULT,
      path: SERVER.SETUP_FROM,
    })

    // WebSocketConfig Mapping
    const webSocket = appDecl.declValue.webSocket
    expect(webSocket).toBeDefined()
    expect(webSocket?.fn).toEqual({
      kind: 'named',
      name: WEBSOCKET.FN_IMPORT,
      path: WEBSOCKET.FROM,
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
      name: ACTION.NAME,
      path: ACTION.FROM,
    })
    expect(actionDecl.declValue.entities).toBeDefined()
    expect(actionDecl.declValue.entities).toHaveLength(1)
    expect(actionDecl.declValue.entities?.[0]).toEqual({
      name: ACTION.ENTITY,
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
    expect(apiNamespaceDecl.declValue.path).toBe(API_NAMESPACE.PATH)
    expect(apiNamespaceDecl.declValue.middlewareConfigFn).toEqual({
      kind: 'named',
      name: API_NAMESPACE.MIDDLEWARE_CONFIG_FN.IMPORT,
      path: API_NAMESPACE.MIDDLEWARE_CONFIG_FN.FROM,
    })

    // ApiConfig Mapping
    expect(declTypes).toContain('Api')
    expect(declNames).toContain(API.NAME)
    const apiDecl = result.find(
      (decl) => decl.declType === 'Api' && decl.declName === API.NAME
    ) as AppSpec.GetDeclForType<'Api'>
    expect(apiDecl.declValue.fn).toEqual({
      kind: 'named',
      name: API.FN.IMPORT,
      path: API.FN.FROM,
    })
    expect(apiDecl.declValue.auth).toBe(API.AUTH)
    expect(apiDecl.declValue.httpRoute).toEqual([
      API.HTTP_ROUTE.METHOD,
      API.HTTP_ROUTE.ROUTE,
    ])
    expect(apiDecl.declValue.entities).toBeDefined()
    expect(apiDecl.declValue.entities).toHaveLength(1)
    expect(apiDecl.declValue.entities?.[0]).toEqual({
      name: API.ENTITY,
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
      name: CRUD.ENTITY,
      declType: 'Entity',
    })
    expect(crudDecl.declValue.operations.get).toBeDefined()
    expect(crudDecl.declValue.operations.get?.isPublic).toBe(
      CRUD.OPERATIONS.GET.IS_PUBLIC
    )
    expect(crudDecl.declValue.operations.get?.overrideFn).toEqual({
      kind: 'named',
      name: CRUD.OPERATIONS.GET.OVERRIDE_FN.IMPORT,
      path: CRUD.OPERATIONS.GET.OVERRIDE_FN.FROM,
    })
    expect(crudDecl.declValue.operations.create).toBeDefined()
    expect(crudDecl.declValue.operations.create?.isPublic).toBe(
      CRUD.OPERATIONS.CREATE.IS_PUBLIC
    )
    expect(crudDecl.declValue.operations.update).toBeDefined()
    expect(crudDecl.declValue.operations.update?.isPublic).toBe(
      CRUD.OPERATIONS.UPDATE.IS_PUBLIC
    )
    expect(crudDecl.declValue.operations.delete).toBeDefined()
    expect(crudDecl.declValue.operations.delete?.isPublic).toBe(
      CRUD.OPERATIONS.DELETE.IS_PUBLIC
    )
    expect(crudDecl.declValue.operations.delete?.overrideFn).toEqual({
      kind: 'named',
      name: CRUD.OPERATIONS.DELETE.OVERRIDE_FN.IMPORT,
      path: CRUD.OPERATIONS.DELETE.OVERRIDE_FN.FROM,
    })

    // JobConfig Mapping
    expect(declTypes).toContain('Job')
    expect(declNames).toContain(JOB.NAME)
    const jobDecl = result.find(
      (decl) => decl.declType === 'Job' && decl.declName === JOB.NAME
    ) as AppSpec.GetDeclForType<'Job'>
    expect(jobDecl.declValue.executor).toBe(JOB.EXECUTOR)
    expect(jobDecl.declValue.perform.fn).toEqual({
      kind: 'named',
      name: JOB.PERFORM.FN.IMPORT,
      path: JOB.PERFORM.FN.FROM,
    })
    expect(jobDecl.declValue.entities?.[0]).toEqual({
      name: JOB.ENTITY,
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
      name: PAGE.LOGIN.COMPONENT.IMPORT,
      path: PAGE.LOGIN.COMPONENT.FROM,
    })
    expect(declNames).toContain(PAGE.EMAIL_VERIFICATION.NAME)
    const emailVerificationPageDecl = result.find(
      (decl) =>
        decl.declType === 'Page' &&
        decl.declName === PAGE.EMAIL_VERIFICATION.NAME
    ) as AppSpec.GetDeclForType<'Page'>
    expect(emailVerificationPageDecl.declValue.component).toEqual({
      kind: 'named',
      name: PAGE.EMAIL_VERIFICATION.COMPONENT.IMPORT,
      path: PAGE.EMAIL_VERIFICATION.COMPONENT.FROM,
    })
    expect(declNames).toContain(PAGE.PASSWORD_RESET.NAME)
    const passwordResetPageDecl = result.find(
      (decl) =>
        decl.declType === 'Page' && decl.declName === PAGE.PASSWORD_RESET.NAME
    ) as AppSpec.GetDeclForType<'Page'>
    expect(passwordResetPageDecl.declValue.component).toEqual({
      kind: 'named',
      name: PAGE.PASSWORD_RESET.COMPONENT.IMPORT,
      path: PAGE.PASSWORD_RESET.COMPONENT.FROM,
    })

    // RouteConfig Mapping
    expect(declTypes).toContain('Route')
    expect(declNames).toContain(ROUTE.LOGIN.NAME)
    const routeDecl = result.find(
      (decl) => decl.declType === 'Route' && decl.declName === ROUTE.LOGIN.NAME
    ) as AppSpec.GetDeclForType<'Route'>
    expect(routeDecl.declValue.path).toBe(ROUTE.LOGIN.PATH)
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
      ROUTE.EMAIL_VERIFICATION.PATH
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
      ROUTE.PASSWORD_RESET.PATH
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
      name: QUERY.NAME,
      path: QUERY.FROM,
    })
    expect(queryDecl.declValue.entities?.[0]).toEqual({
      name: QUERY.ENTITY,
      declType: 'Entity',
    })
  })
})
