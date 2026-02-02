/**
 * This module maps the TsAppSpec-facing API to the internal representation of the app (AppSpec Decl).
 * All of the mapping functions are exported so that they can be individually tested.
 */

import * as AppSpec from "./appSpec.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

export function mapTsAppSpecToAppSpecDecls(
  tsAppSpec: TsAppSpec.TsAppSpec,
  entityNames: string[],
): AppSpec.Decl[] {
  const {
    app,
    actions,
    apis,
    apiNamespaces,
    auth,
    client,
    db,
    emailSender,
    jobs,
    pages,
    queries,
    routes,
    server,
    websocket,
    cruds,
  } = tsAppSpec;

  const pageNames = Array.from(pages.keys());
  const routeNames = Array.from(routes.keys());

  const entityRefParser = makeRefParser("Entity", entityNames);
  const pageRefParser = makeRefParser("Page", pageNames);
  const routeRefParser = makeRefParser("Route", routeNames);

  const pageDecls = mapToDecls(pages, "Page", mapPage);
  const routeDecls = mapToDecls(routes, "Route", (routeConfig) =>
    mapRoute(routeConfig, pageRefParser),
  );
  const actionDecls = mapToDecls(actions, "Action", (actionConfig) =>
    mapOperation(actionConfig, entityRefParser),
  );
  const queryDecls = mapToDecls(queries, "Query", (queryConfig) =>
    mapOperation(queryConfig, entityRefParser),
  );
  const apiDecls = mapToDecls(apis, "Api", (apiConfig) =>
    mapApi(apiConfig, entityRefParser),
  );
  const jobDecls = mapToDecls(jobs, "Job", (jobConfig) =>
    mapJob(jobConfig, entityRefParser),
  );
  const apiNamespaceDecls = mapToDecls(
    apiNamespaces,
    "ApiNamespace",
    mapApiNamespace,
  );
  const crudDecls = mapToDecls(cruds, "Crud", (crudConfig) =>
    mapCrud(crudConfig, entityRefParser),
  );

  const appDecl = {
    declType: "App" as const,
    declName: app.name,
    declValue: mapApp(
      app.config,
      entityRefParser,
      routeRefParser,
      auth,
      client,
      server,
      db,
      emailSender,
      websocket,
    ),
  };

  return makeDeclsArray({
    App: [appDecl],
    Page: pageDecls,
    Route: routeDecls,
    Action: actionDecls,
    Query: queryDecls,
    Api: apiDecls,
    Job: jobDecls,
    ApiNamespace: apiNamespaceDecls,
    Crud: crudDecls,
  });
}

function makeDeclsArray(decls: {
  [Type in AppSpec.Decl["declType"]]: AppSpec.GetDeclForType<Type>[];
}): AppSpec.Decl[] {
  return Object.values(decls).flatMap((decl) => [...decl]);
}

function mapToDecls<T, DeclType extends AppSpec.Decl["declType"]>(
  configs: Map<string, T>,
  type: DeclType,
  configToDeclValue: (
    config: T,
  ) => AppSpec.GetDeclForType<DeclType>["declValue"],
) {
  return [...configs].map(([name, config]) => ({
    declType: type,
    declName: name,
    declValue: configToDeclValue(config),
  }));
}

export function mapOperation(
  config: TsAppSpec.QueryConfig,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Query;
export function mapOperation(
  config: TsAppSpec.ActionConfig,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Action;
export function mapOperation(
  config: TsAppSpec.ActionConfig | TsAppSpec.QueryConfig,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Action | AppSpec.Query {
  const { fn, entities, auth } = config;
  return {
    fn: mapExtImport(fn),
    entities: entities?.map(entityRefParser),
    auth,
  };
}

export function mapExtImport(
  extImport: TsAppSpec.ExtImport,
): AppSpec.ExtImport {
  if ("import" in extImport) {
    return {
      kind: "named",
      name: extImport.import,
      path: extImport.from,
    };
  } else if ("importDefault" in extImport) {
    return {
      kind: "default",
      name: extImport.importDefault,
      path: extImport.from,
    };
  } else {
    throw new Error(
      "Invalid ExtImport: neither `import` nor `importDefault` is defined",
    );
  }
}

export function mapHttpRoute(
  httpRoute: TsAppSpec.HttpRoute,
): AppSpec.HttpRoute {
  return [httpRoute.method, httpRoute.route];
}

export function mapApi(
  config: TsAppSpec.ApiConfig,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Api {
  const { fn, middlewareConfigFn, entities, httpRoute, auth } = config;
  return {
    fn: mapExtImport(fn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    entities: entities && entities.map(entityRefParser),
    httpRoute: mapHttpRoute(httpRoute),
    auth,
  };
}

export function mapApiNamespace(
  config: TsAppSpec.ApiNamespaceConfig,
): AppSpec.ApiNamespace {
  const { middlewareConfigFn, path } = config;
  return {
    middlewareConfigFn: mapExtImport(middlewareConfigFn),
    path,
  };
}

export function mapApp(
  app: TsAppSpec.AppConfig,
  // TODO: Make this better, optional props are problematic so I have to pass the parsers first
  entityRefParser: RefParser<"Entity">,
  routeRefParser: RefParser<"Route">,
  auth?: TsAppSpec.AuthConfig,
  client?: TsAppSpec.ClientConfig,
  server?: TsAppSpec.ServerConfig,
  db?: TsAppSpec.DbConfig,
  emailSender?: TsAppSpec.EmailSenderConfig,
  webSocket?: TsAppSpec.WebsocketConfig,
): AppSpec.App {
  const { title, wasp, head } = app;
  return {
    wasp,
    title,
    head,
    auth: auth && mapAuth(auth, entityRefParser, routeRefParser),
    client: client && mapClient(client),
    server: server && mapServer(server),
    webSocket: webSocket && mapWebSocket(webSocket),
    db: db && mapDb(db),
    emailSender: emailSender && mapEmailSender(emailSender),
  };
}

export function mapAuth(
  auth: TsAppSpec.AuthConfig,
  entityRefParser: RefParser<"Entity">,
  routeRefParser: RefParser<"Route">,
): AppSpec.Auth {
  const {
    userEntity,
    externalAuthEntity,
    methods,
    onAuthFailedRedirectTo,
    onAuthSucceededRedirectTo,
    onBeforeSignup,
    onAfterSignup,
    onAfterEmailVerified,
    onBeforeOAuthRedirect,
    onBeforeLogin,
    onAfterLogin,
  } = auth;
  return {
    userEntity: entityRefParser(userEntity),
    // TODO: Abstract away this pattern
    externalAuthEntity:
      externalAuthEntity === undefined
        ? undefined
        : entityRefParser(externalAuthEntity),
    methods: mapAuthMethods(methods, routeRefParser),
    onAuthFailedRedirectTo,
    onAuthSucceededRedirectTo,
    onBeforeSignup: onBeforeSignup && mapExtImport(onBeforeSignup),
    onAfterSignup: onAfterSignup && mapExtImport(onAfterSignup),
    onAfterEmailVerified:
      onAfterEmailVerified && mapExtImport(onAfterEmailVerified),
    onBeforeOAuthRedirect:
      onBeforeOAuthRedirect && mapExtImport(onBeforeOAuthRedirect),
    onBeforeLogin: onBeforeLogin && mapExtImport(onBeforeLogin),
    onAfterLogin: onAfterLogin && mapExtImport(onAfterLogin),
  };
}

export function mapAuthMethods(
  methods: TsAppSpec.AuthMethods,
  routeRefParser: RefParser<"Route">,
): AppSpec.AuthMethods {
  // TODO: check keyof danger, effective ts
  const { usernameAndPassword, discord, google, gitHub, keycloak, email } =
    methods;
  return {
    usernameAndPassword:
      usernameAndPassword && mapUsernameAndPassword(usernameAndPassword),
    discord: discord && mapExternalAuth(discord),
    google: google && mapExternalAuth(google),
    gitHub: gitHub && mapExternalAuth(gitHub),
    keycloak: keycloak && mapExternalAuth(keycloak),
    email: email && mapEmailAuth(email, routeRefParser),
  };
}

export function mapUsernameAndPassword(
  usernameAndPassword: TsAppSpec.UsernameAndPasswordConfig,
): AppSpec.UsernameAndPasswordConfig {
  const { userSignupFields } = usernameAndPassword;
  return {
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
  };
}

export function mapExternalAuth(
  externalAuth: TsAppSpec.ExternalAuthConfig,
): AppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = externalAuth;
  return {
    configFn: configFn && mapExtImport(configFn),
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
  };
}

export function mapEmailAuth(
  emailConfig: TsAppSpec.EmailAuthConfig,
  routeRefParser: RefParser<"Route">,
): AppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    emailConfig;
  return {
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
    fromField: mapEmailFromField(fromField),
    emailVerification: mapEmailVerification(emailVerification, routeRefParser),
    passwordReset: mapPasswordReset(passwordReset, routeRefParser),
  };
}

export function mapEmailVerification(
  emailVerification: TsAppSpec.EmailVerificationConfig,
  routeRefParser: RefParser<"Route">,
): AppSpec.EmailVerificationConfig {
  const { getEmailContentFn, clientRoute } = emailVerification;
  return {
    getEmailContentFn: getEmailContentFn && mapExtImport(getEmailContentFn),
    clientRoute: routeRefParser(clientRoute),
  };
}

export function mapPasswordReset(
  passwordReset: TsAppSpec.PasswordResetConfig,
  routeRefParser: RefParser<"Route">,
): AppSpec.PasswordResetConfig {
  const { getEmailContentFn, clientRoute } = passwordReset;
  return {
    getEmailContentFn: getEmailContentFn && mapExtImport(getEmailContentFn),
    clientRoute: routeRefParser(clientRoute),
  };
}

export function mapDb(db: TsAppSpec.DbConfig): AppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(mapExtImport),
    prismaSetupFn: prismaSetupFn && mapExtImport(prismaSetupFn),
  };
}

export function mapEmailSender(
  emailSender: TsAppSpec.EmailSenderConfig,
): AppSpec.EmailSender {
  const { provider, defaultFrom } = emailSender;
  return {
    provider,
    defaultFrom: defaultFrom && mapEmailFromField(defaultFrom),
  };
}

export function mapEmailFromField(
  defaultFrom: TsAppSpec.EmailFromField,
): AppSpec.EmailFromField {
  return (
    defaultFrom && {
      name: defaultFrom.name,
      email: defaultFrom.email,
    }
  );
}

export function mapServer(server: TsAppSpec.ServerConfig): AppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server;
  return {
    setupFn: setupFn && mapExtImport(setupFn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  };
}

export function mapClient(client: TsAppSpec.ClientConfig): AppSpec.Client {
  const { setupFn, rootComponent, baseDir, envValidationSchema } = client;
  return {
    setupFn: setupFn && mapExtImport(setupFn),
    rootComponent: rootComponent && mapExtImport(rootComponent),
    baseDir,
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  };
}

export function mapWebSocket(
  websocket: TsAppSpec.WebsocketConfig,
): AppSpec.WebSocket {
  const { fn, autoConnect } = websocket;
  return {
    fn: mapExtImport(fn),
    autoConnect,
  };
}

export function mapJob(
  job: TsAppSpec.JobConfig,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Job {
  const { executor, perform, schedule, entities } = job;
  return {
    executor,
    perform: mapPerform(perform),
    schedule: schedule && mapSchedule(schedule),
    entities: entities && entities.map(entityRefParser),
  };
}

export function mapSchedule(schedule: TsAppSpec.Schedule): AppSpec.Schedule {
  const { cron, args, executorOptions } = schedule;
  return {
    cron,
    args,
    executorOptions,
  };
}

export function mapPerform(perform: TsAppSpec.Perform): AppSpec.Perform {
  const { fn, executorOptions } = perform;
  return {
    fn: mapExtImport(fn),
    executorOptions,
  };
}

export function mapRoute(
  route: TsAppSpec.RouteConfig,
  pageRefParser: RefParser<"Page">,
): AppSpec.Route {
  const { path, to } = route;
  return {
    path,
    to: pageRefParser(to),
  };
}

export function mapCrud(
  crudConfig: TsAppSpec.CrudConfig,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Crud {
  const { entity, operations } = crudConfig;
  return {
    entity: entityRefParser(entity),
    operations: mapCrudOperations(operations),
  };
}

export function mapCrudOperations(
  operations: TsAppSpec.CrudOperations,
): AppSpec.CrudOperations {
  const { get, getAll, create, update, delete: del } = operations;
  // TODO: Do this for all keys
  return {
    get: get && mapCrudOperationOptions(get),
    getAll: getAll && mapCrudOperationOptions(getAll),
    create: create && mapCrudOperationOptions(create),
    update: update && mapCrudOperationOptions(update),
    delete: del && mapCrudOperationOptions(del),
  };
}

export function mapCrudOperationOptions(
  options: TsAppSpec.CrudOperationOptions,
): AppSpec.CrudOperationOptions {
  const { isPublic, overrideFn } = options;
  return {
    isPublic,
    overrideFn: overrideFn && mapExtImport(overrideFn),
  };
}

export function mapPage(pageConfig: TsAppSpec.PageConfig): AppSpec.Page {
  const { component, authRequired } = pageConfig;
  return {
    component: mapExtImport(component),
    authRequired,
  };
}

export type RefParser<T extends AppSpec.DeclType> = (
  potentialReferences: string,
) => AppSpec.Ref<T>;

export function makeRefParser<T extends AppSpec.DeclType>(
  declType: T,
  declNames: string[],
): RefParser<T> {
  return function parseRef(potentialRef: string): AppSpec.Ref<T> {
    if (!declNames.includes(potentialRef)) {
      throw new Error(`Invalid ${declType} reference: ${potentialRef}`);
    }
    return {
      name: potentialRef,
      declType,
    } as AppSpec.Ref<T>;
  };
}
