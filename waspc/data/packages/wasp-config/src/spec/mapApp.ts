/**
 * This module maps the TsAppSpec-facing API to the internal representation of the app (AppSpec Decl).
 * All of the mapping functions are exported so that they can be individually tested.
 */

import * as AppSpec from "../appSpec.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

export function mapApp(
  app: TsAppSpec.App,
  entityNames: string[],
): AppSpec.Decl[] {
  const {
    name,
    wasp,
    title,
    head,
    auth,
    server,
    client,
    db,
    emailSender,
    webSocket,
    parts,
  } = app;

  const entityRefParser = makeRefParser("Entity", entityNames);

  // TODO: When you add all declarations, see if you can generalize better
  // (e.g., maybe named parameters, maybe putting extractParts inside
  // mapToDecls)
  const pages = extractParts("page", parts);
  const pageDecls = mapToDecls(
    pages,
    "Page",
    (page) => deriveExtImportName(page.component),
    mapPage,
  );

  const routes = extractParts("route", parts);
  const routeDecls = mapToDecls(
    routes,
    "Route",
    (route) => route.name,
    (route) => mapRoute(route),
  );
  const routePageDecls = mapToDecls(
    routes,
    "Page",
    (route) => deriveExtImportName(route.page.component),
    (route) => mapPage(route.page),
  );
  const routeRefParser = makeRefParser(
    "Route",
    routes.map((r) => r.name),
  );

  const queries = extractParts("query", parts);
  const queryDecls = mapToDecls(
    queries,
    "Query",
    (query) => deriveExtImportName(query.fn),
    (query) => mapQuery(query, entityRefParser),
  );

  const actions = extractParts("action", parts);
  const actionDecls = mapToDecls(
    actions,
    "Action",
    (action) => deriveExtImportName(action.fn),
    (action) => mapAction(action, entityRefParser),
  );

  const apis = extractParts("api", parts);
  const apiDecls = mapToDecls(
    apis,
    "Api",
    (api) => deriveExtImportName(api.fn),
    (api) => mapApi(api, entityRefParser),
  );

  const apiNamespaces = extractParts("apiNamespace", parts);
  const apiNamespaceDecls = mapToDecls(
    apiNamespaces,
    "ApiNamespace",
    (ns) => deriveExtImportName(ns.middlewareConfigFn),
    mapApiNamespace,
  );

  const jobs = extractParts("job", parts);
  const jobDecls = mapToDecls(
    jobs,
    "Job",
    (job) => deriveExtImportName(job.fn),
    (job) => mapJob(job, entityRefParser),
  );

  const cruds = extractParts("crud", parts);
  const crudDecls = mapToDecls(
    cruds,
    "Crud",
    (crud) => `${crud.entity}s`,
    (crud) => mapCrud(crud, entityRefParser),
  );

  const appDecl = {
    declType: "App" as const,
    declName: name,
    declValue: {
      wasp,
      title,
      head,
      auth: auth && mapAuth(auth, entityRefParser, routeRefParser),
      server: server && mapServer(server),
      client: client && mapClient(client),
      db: db && mapDb(db),
      emailSender: emailSender && mapEmailSender(emailSender),
      webSocket: webSocket && mapWebSocket(webSocket),
    },
  };

  return makeDeclsArray({
    App: [appDecl],
    Page: dedupePageDecls([...pageDecls, ...routePageDecls]),
    Route: routeDecls,
    Query: queryDecls,
    Action: actionDecls,
    Api: apiDecls,
    ApiNamespace: apiNamespaceDecls,
    Job: jobDecls,
    Crud: crudDecls,
  });
}

export function mapPage(page: TsAppSpec.Page): AppSpec.Page {
  const { component, authRequired } = page;
  return {
    component: mapExtImport(component),
    authRequired,
  };
}

export function mapRoute(route: TsAppSpec.Route): AppSpec.Route {
  const { path, prerender, lazy } = route;
  return {
    path,
    to: {
      name: deriveExtImportName(route.page.component),
      declType: "Page",
    },
    prerender,
    lazy,
  };
}

/**
 * {@link TsAppSpec.Route} through it's constructor can either:
 * - Create a new {@link TsAppSpec.Page}.
 * - Reference an existing {@link TsAppSpec.Page}.
 *
 * In case when it references an existing page, we don't want to
 * count the reference as a separate {@link AppSpec.Page} declaration.
 */
export function dedupePageDecls(
  decls: AppSpec.GetDeclForType<"Page">[],
): AppSpec.GetDeclForType<"Page">[] {
  const pagesByDeclName = Map.groupBy(decls, (decls) => decls.declName);
  return Array.from(pagesByDeclName.values()).map((pages) =>
    pages.reduce((firstPage, currentPage) => {
      if (!arePageDeclsEqual(currentPage, firstPage)) {
        throw new Error(
          `Conflicting configs for page "${firstPage.declName}". ` +
            "All page instances pointing to the same component must produce the same configuration.\n\n" +
            `Page 1: ${JSON.stringify(firstPage.declValue)}\n` +
            `Page 2: ${JSON.stringify(currentPage.declValue)}`,
        );
      }
      return firstPage;
    }),
  );
}

function arePageDeclsEqual(
  page1: AppSpec.GetDeclForType<"Page">,
  page2: AppSpec.GetDeclForType<"Page">,
): boolean {
  const isSameCanonicalPage = page1.declName === page2.declName;
  return (
    isSameCanonicalPage &&
    JSON.stringify(page1.declValue) === JSON.stringify(page2.declValue)
  );
}

export function mapQuery(
  query: TsAppSpec.Query,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Query {
  const { fn, entities, auth } = query;
  return {
    fn: mapExtImport(fn),
    entities: entities?.map(entityRefParser),
    auth,
  };
}

export function mapAction(
  action: TsAppSpec.Action,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Action {
  const { fn, entities, auth } = action;
  return {
    fn: mapExtImport(fn),
    entities: entities?.map(entityRefParser),
    auth,
  };
}

export function mapAuth(
  auth: TsAppSpec.Auth,
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
  const {
    usernameAndPassword,
    discord,
    google,
    gitHub,
    keycloak,
    microsoft,
    email,
  } = methods;
  return {
    usernameAndPassword:
      usernameAndPassword && mapUsernameAndPassword(usernameAndPassword),
    discord: discord && mapExternalAuth(discord),
    google: google && mapExternalAuth(google),
    gitHub: gitHub && mapExternalAuth(gitHub),
    keycloak: keycloak && mapExternalAuth(keycloak),
    microsoft: microsoft && mapExternalAuth(microsoft),
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
  emailAuth: TsAppSpec.EmailAuthConfig,
  routeRefParser: RefParser<"Route">,
): AppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    emailAuth;
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

export function mapApi(
  api: TsAppSpec.Api,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Api {
  const { method, path, fn, middlewareConfigFn, entities, auth } = api;
  return {
    fn: mapExtImport(fn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    entities: entities?.map(entityRefParser),
    httpRoute: [method, path],
    auth,
  };
}

export function mapApiNamespace(
  apiNamespace: TsAppSpec.ApiNamespace,
): AppSpec.ApiNamespace {
  const { middlewareConfigFn, path } = apiNamespace;
  return {
    middlewareConfigFn: mapExtImport(middlewareConfigFn),
    path,
  };
}

export function mapServer(server: TsAppSpec.Server): AppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server;
  return {
    setupFn: setupFn && mapExtImport(setupFn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  };
}

export function mapClient(client: TsAppSpec.Client): AppSpec.Client {
  const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
  return {
    rootComponent: rootComponent && mapExtImport(rootComponent),
    setupFn: setupFn && mapExtImport(setupFn),
    baseDir,
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  };
}

export function mapDb(db: TsAppSpec.Db): AppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(mapExtImport),
    prismaSetupFn: prismaSetupFn && mapExtImport(prismaSetupFn),
  };
}

export function mapEmailSender(
  emailSender: TsAppSpec.EmailSender,
): AppSpec.EmailSender {
  const { provider, defaultFrom } = emailSender;
  return {
    provider,
    defaultFrom: defaultFrom && mapEmailFromField(defaultFrom),
  };
}

export function mapEmailFromField(
  emailFromField: TsAppSpec.EmailFromField,
): AppSpec.EmailFromField {
  return {
    name: emailFromField.name,
    email: emailFromField.email,
  };
}

export function mapWebSocket(
  webSocket: TsAppSpec.WebSocket,
): AppSpec.WebSocket {
  const { fn, autoConnect } = webSocket;
  return {
    fn: mapExtImport(fn),
    autoConnect,
  };
}

export function mapJob(
  job: TsAppSpec.Job,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Job {
  const { fn, executor, schedule, entities, performExecutorOptions } = job;
  return {
    executor,
    perform: {
      fn: mapExtImport(fn),
      executorOptions: performExecutorOptions,
    },
    schedule: schedule && mapSchedule(schedule),
    entities: entities?.map(entityRefParser),
  };
}

export function mapCrud(
  crud: TsAppSpec.Crud,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Crud {
  const { entity, operations } = crud;
  return {
    entity: entityRefParser(entity),
    operations: mapCrudOperations(operations),
  };
}

export function mapCrudOperations(
  operations: TsAppSpec.CrudOperations,
): AppSpec.CrudOperations {
  const { get, getAll, create, update, delete: del } = operations;
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

export function mapSchedule(schedule: TsAppSpec.Schedule): AppSpec.Schedule {
  const { cron, args, executorOptions } = schedule;
  return {
    cron,
    args,
    executorOptions,
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

export type RefParser<T extends AppSpec.DeclType> = (
  name: string,
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
    };
  };
}

function extractParts<Kind extends TsAppSpec.Part["kind"]>(
  id: Kind,
  parts: TsAppSpec.Part[],
): GetPartForKind<Kind>[] {
  return parts.filter((p): p is GetPartForKind<Kind> => p.kind === id);
}

type GetPartForKind<Kind extends TsAppSpec.Part["kind"]> = Extract<
  TsAppSpec.Part,
  { kind: Kind }
>;

function mapToDecls<T, DeclType extends AppSpec.Decl["declType"]>(
  items: T[],
  declType: DeclType,
  deriveName: (item: T) => string,
  mapValue: (item: T) => AppSpec.GetDeclForType<DeclType>["declValue"],
) {
  return items.map((item) => ({
    declType,
    declName: deriveName(item),
    declValue: mapValue(item),
  }));
}

export function deriveExtImportName(extImport: TsAppSpec.ExtImport): string {
  if ("import" in extImport) {
    return extImport.alias ?? extImport.import;
  }
  return extImport.importDefault;
}

/**
 * The point of this function is to enforce exhaustivness over all declaration
 * types, ensuring we don't forget to include anything.
 * Check the original comment for details: https://github.com/wasp-lang/wasp/pull/2393#discussion_r1866620833
 *
 * TODO: The new spec bundles all parts (queries, actions...) together in the parts array, so
 * there's no need to go through them one by one.
 * We'd likely be better of by:
 *   1. Mapping the entire array with a dispatcher that calls the correct
 *   mapper depending on the part's kind
 *   2. Passing this mapped array into the app spec (which expects them all on
 *   the same level anyway).
 * We'll likely lose some mapping type safety in the process though. Explore
 * when we're done with the port from legacy to the new spec.
 */
function makeDeclsArray(decls: {
  [Type in AppSpec.Decl["declType"]]: AppSpec.GetDeclForType<Type>[];
}): AppSpec.Decl[] {
  return Object.values(decls).flatMap((decl) => [...decl]);
}
