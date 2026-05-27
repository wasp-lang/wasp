/**
 * This module maps the TsAppSpec-facing API to the internal representation of the app (AppSpec Decl).
 * All of the mapping functions are exported so that they can be individually tested.
 */

import * as AppSpec from "../appSpec.js";
import type { AnyFunction } from "../typeUtils.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";
import { mapRefObject } from "./refObject.js";
import { SpecUserError } from "./specUserError.js";

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
    (page) => deriveRefObjectName(page.component),
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
    (route) => deriveRefObjectName(route.page.component),
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
    (query) => deriveRefObjectName(query.fn),
    (query) => mapQuery(query, entityRefParser),
  );

  const actions = extractParts("action", parts);
  const actionDecls = mapToDecls(
    actions,
    "Action",
    (action) => deriveRefObjectName(action.fn),
    (action) => mapAction(action, entityRefParser),
  );

  const apis = extractParts("api", parts);
  const apiDecls = mapToDecls(
    apis,
    "Api",
    (api) => deriveRefObjectName(api.fn),
    (api) => mapApi(api, entityRefParser),
  );

  const apiNamespaces = extractParts("apiNamespace", parts);
  const apiNamespaceDecls = mapToDecls(
    apiNamespaces,
    "ApiNamespace",
    (ns) => deriveRefObjectName(ns.middlewareConfigFn),
    mapApiNamespace,
  );

  const jobs = extractParts("job", parts);
  const jobDecls = mapToDecls(
    jobs,
    "Job",
    (job) => deriveRefObjectName(job.fn),
    (job) => mapJob(job, entityRefParser),
  );

  const cruds = extractParts("crud", parts);
  const crudDecls = mapToDecls(
    cruds,
    "Crud",
    (crud) => crud.name,
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
    component: mapRefObject(component),
    authRequired,
  };
}

export function mapRoute(route: TsAppSpec.Route): AppSpec.Route {
  const { path, prerender, lazy } = route;
  return {
    path,
    to: {
      name: deriveRefObjectName(route.page.component),
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
        throw new SpecUserError(
          `Conflicting configurations for the page \`${firstPage.declName}\`:\n` +
            `- Definition A: ${JSON.stringify(firstPage.declValue)}\n` +
            `- Definition B: ${JSON.stringify(currentPage.declValue)}\n\n` +
            "All page instances with the same import name must produce the same configuration.\n" +
            "If the duplication was intentional, please use an alias to differentiate the pages.",
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
    fn: mapRefObject(fn),
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
    fn: mapRefObject(fn),
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
    methods: mapAuthMethods(methods, routeRefParser),
    onAuthFailedRedirectTo,
    onAuthSucceededRedirectTo,
    onBeforeSignup: onBeforeSignup && mapRefObject(onBeforeSignup),
    onAfterSignup: onAfterSignup && mapRefObject(onAfterSignup),
    onAfterEmailVerified:
      onAfterEmailVerified && mapRefObject(onAfterEmailVerified),
    onBeforeOAuthRedirect:
      onBeforeOAuthRedirect && mapRefObject(onBeforeOAuthRedirect),
    onBeforeLogin: onBeforeLogin && mapRefObject(onBeforeLogin),
    onAfterLogin: onAfterLogin && mapRefObject(onAfterLogin),
  };
}

export function mapAuthMethods(
  methods: TsAppSpec.AuthMethods,
  routeRefParser: RefParser<"Route">,
): AppSpec.AuthMethods {
  const {
    usernameAndPassword,
    slack,
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
    slack: slack && mapSocialAuth(slack),
    discord: discord && mapSocialAuth(discord),
    google: google && mapSocialAuth(google),
    gitHub: gitHub && mapSocialAuth(gitHub),
    keycloak: keycloak && mapSocialAuth(keycloak),
    microsoft: microsoft && mapSocialAuth(microsoft),
    email: email && mapEmailAuth(email, routeRefParser),
  };
}

export function mapUsernameAndPassword(
  usernameAndPassword: TsAppSpec.UsernameAndPasswordConfig,
): AppSpec.UsernameAndPasswordConfig {
  const { userSignupFields } = usernameAndPassword;
  return {
    userSignupFields: userSignupFields && mapRefObject(userSignupFields),
  };
}

export function mapSocialAuth(
  socialAuth: TsAppSpec.SocialAuthConfig,
): AppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = socialAuth;
  return {
    configFn: configFn && mapRefObject(configFn),
    userSignupFields: userSignupFields && mapRefObject(userSignupFields),
  };
}

export function mapEmailAuth(
  emailAuth: TsAppSpec.EmailAuthConfig,
  routeRefParser: RefParser<"Route">,
): AppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    emailAuth;
  return {
    userSignupFields: userSignupFields && mapRefObject(userSignupFields),
    fromField: mapEmailFromField(fromField),
    emailVerification: mapEmailFlow(emailVerification, routeRefParser),
    passwordReset: mapEmailFlow(passwordReset, routeRefParser),
  };
}

export function mapEmailFlow(
  emailFlow: TsAppSpec.EmailFlowConfig,
  routeRefParser: RefParser<"Route">,
): AppSpec.EmailVerificationConfig {
  const { getEmailContentFn, clientRoute } = emailFlow;
  return {
    getEmailContentFn: getEmailContentFn && mapRefObject(getEmailContentFn),
    clientRoute: routeRefParser(clientRoute),
  };
}

export function mapApi(
  api: TsAppSpec.Api,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Api {
  const { method, path, fn, middlewareConfigFn, entities, auth } = api;
  return {
    fn: mapRefObject(fn),
    middlewareConfigFn: middlewareConfigFn && mapRefObject(middlewareConfigFn),
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
    middlewareConfigFn: mapRefObject(middlewareConfigFn),
    path,
  };
}

export function mapServer(server: TsAppSpec.Server): AppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server;
  return {
    setupFn: setupFn && mapRefObject(setupFn),
    middlewareConfigFn: middlewareConfigFn && mapRefObject(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && mapRefObject(envValidationSchema),
  };
}

export function mapClient(client: TsAppSpec.Client): AppSpec.Client {
  const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
  return {
    rootComponent: rootComponent && mapRefObject(rootComponent),
    setupFn: setupFn && mapRefObject(setupFn),
    baseDir,
    envValidationSchema:
      envValidationSchema && mapRefObject(envValidationSchema),
  };
}

export function mapDb(db: TsAppSpec.Db): AppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(mapRefObject),
    prismaSetupFn: prismaSetupFn && mapRefObject(prismaSetupFn),
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
    fn: mapRefObject(fn),
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
      fn: mapRefObject(fn),
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
    overrideFn: overrideFn && mapRefObject(overrideFn),
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

export type RefParser<T extends AppSpec.DeclType> = (
  name: string,
) => AppSpec.Ref<T>;

export function makeRefParser<T extends AppSpec.DeclType>(
  declType: T,
  declNames: string[],
): RefParser<T> {
  return function parseRef(potentialRef: string): AppSpec.Ref<T> {
    if (!declNames.includes(potentialRef)) {
      throw new SpecUserError(
        `Invalid \`${declType}\` reference: \`${potentialRef}\`\n` +
          `Please make sure that \`${potentialRef}\` is actually defined.`,
      );
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

export function deriveRefObjectName(
  refObject: TsAppSpec.RefObject | AnyFunction,
): string {
  const mappedRefObject = mapRefObject(refObject);

  return "alias" in mappedRefObject
    ? (mappedRefObject.alias ?? mappedRefObject.name)
    : mappedRefObject.name;
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
