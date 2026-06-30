/**
 * This module maps the Wasp Spec to the internal representation of
 * the app ({@link AppSpec.Decl}).
 * All of the mapping functions are exported so that they can be individually
 * tested.
 */

import * as AppSpec from "../appSpec.js";
import { normalizePrerender } from "../normalizePrerender.js";
import * as WaspSpec from "./publicApi/waspSpec.js";
import { getRefObjectDeclarationName, mapRefObject } from "./refObject.js";
import { SpecUserError } from "./specUserError.js";

export function mapApp(
  app: WaspSpec.App,
  {
    projectRootDir,
    entityNames,
  }: {
    projectRootDir: string;
    entityNames: string[];
  },
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
    spec,
  } = app;
  const flatSpec = flattenSpec(spec);

  const entityRefParser = makeRefParser("Entity", entityNames);
  // Routes referenced by `auth` (e.g. `onAuthFailedRedirectTo`) are
  // auto-registered, mirroring how `route(_, _, page())` auto-registers the
  // page passed to it. A route that is both listed in `spec` and referenced by
  // `auth` is registered once (see `dedupeRouteDecls`).
  const routeElements = [
    ...extractSpecElements("route", flatSpec),
    ...(auth ? collectAuthRoutes(auth) : []),
  ];
  const routeRefParser = makeRefParser(
    "Route",
    routeElements.map((r) => r.name),
  );
  const ctx: AppMapperContext = {
    entityRefParser,
    routeRefParser,
    mapRefObject: (refObject: unknown) =>
      mapRefObject(refObject, {
        projectRootDir,
      }),
  };

  // TODO: When you add all declarations, see if you can generalize better
  // (e.g., maybe named parameters, maybe putting extractSpecElements inside
  // mapToDecls)
  const pageSpecElements = extractSpecElements("page", flatSpec);
  const pageDecls = mapToDecls(
    pageSpecElements,
    "Page",
    (page) => getRefObjectDeclarationName(page.component),
    (page) => mapPage(page, ctx),
  );

  const routeDecls = mapToDecls(
    routeElements,
    "Route",
    (route) => route.name,
    (route) => mapRoute(route),
  );
  const routePageDecls = mapToDecls(
    routeElements,
    "Page",
    (route) => getRefObjectDeclarationName(route.page.component),
    (route) => mapPage(route.page, ctx),
  );

  const querySpecElements = extractSpecElements("query", flatSpec);
  const queryDecls = mapToDecls(
    querySpecElements,
    "Query",
    (query) => getRefObjectDeclarationName(query.fn),
    (query) => mapQuery(query, ctx),
  );

  const actionSpecElements = extractSpecElements("action", flatSpec);
  const actionDecls = mapToDecls(
    actionSpecElements,
    "Action",
    (action) => getRefObjectDeclarationName(action.fn),
    (action) => mapAction(action, ctx),
  );

  const apiSpecElements = extractSpecElements("api", flatSpec);
  const apiDecls = mapToDecls(
    apiSpecElements,
    "Api",
    (api) => getRefObjectDeclarationName(api.fn),
    (api) => mapApi(api, ctx),
  );

  const apiNamespaceSpecElements = extractSpecElements(
    "apiNamespace",
    flatSpec,
  );
  const apiNamespaceDecls = mapToDecls(
    apiNamespaceSpecElements,
    "ApiNamespace",
    (ns) => getRefObjectDeclarationName(ns.middlewareConfigFn),
    (ns) => mapApiNamespace(ns, ctx),
  );

  const jobSpecElements = extractSpecElements("job", flatSpec);
  const jobDecls = mapToDecls(
    jobSpecElements,
    "Job",
    (job) => getRefObjectDeclarationName(job.fn),
    (job) => mapJob(job, ctx),
  );

  const crudSpecElements = extractSpecElements("crud", flatSpec);
  const crudDecls = mapToDecls(
    crudSpecElements,
    "Crud",
    (crud) => crud.name,
    (crud) => mapCrud(crud, ctx),
  );

  const appDecl = {
    declType: "App" as const,
    declName: name,
    declValue: {
      wasp,
      title,
      head,
      auth: auth && mapAuth(auth, ctx),
      server: server && mapServer(server, ctx),
      client: client && mapClient(client, ctx),
      db: db && mapDb(db, ctx),
      emailSender: emailSender && mapEmailSender(emailSender),
      webSocket: webSocket && mapWebSocket(webSocket, ctx),
    },
  };

  return ensureAllDecls({
    App: [appDecl],
    Page: dedupePageDecls([...pageDecls, ...routePageDecls]),
    Route: dedupeRouteDecls(routeDecls),
    Query: queryDecls,
    Action: actionDecls,
    Api: apiDecls,
    ApiNamespace: apiNamespaceDecls,
    Job: jobDecls,
    Crud: crudDecls,
  });
}

export type AppMapperContext = {
  entityRefParser: RefParser<"Entity">;
  routeRefParser: RefParser<"Route">;
  mapRefObject: RefObjectMapper;
};

type RefObjectMapper = (refObject: unknown) => AppSpec.ExtImport;

export function mapPage(
  page: WaspSpec.Page,
  ctx: AppMapperContext,
): AppSpec.Page {
  const { component, authRequired } = page;
  return {
    component: ctx.mapRefObject(component),
    authRequired,
  };
}

export function mapRoute(route: WaspSpec.Route): AppSpec.Route {
  const { path, prerender, lazy } = route;
  return {
    path,
    to: {
      name: getRefObjectDeclarationName(route.page.component),
      declType: "Page",
    },
    prerender: normalizePrerender(prerender, path),
    lazy,
  };
}

/**
 * {@link WaspSpec.Route} through it's constructor can either:
 * - Create a new {@link WaspSpec.Page}.
 * - Reference an existing {@link WaspSpec.Page}.
 *
 * In case when it references an existing page, we don't want to
 * count the reference as a separate {@link AppSpec.Page} specification.
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

/**
 * A {@link WaspSpec.Route} can be both listed in the app's `spec` and
 * referenced by `auth` (e.g. {@link WaspSpec.Auth.onAuthFailedRedirectTo}). We
 * register each route once, collapsing identical declarations and rejecting
 * conflicting ones that share a name.
 */
export function dedupeRouteDecls(
  decls: AppSpec.GetDeclForType<"Route">[],
): AppSpec.GetDeclForType<"Route">[] {
  const routesByDeclName = Map.groupBy(decls, (decl) => decl.declName);
  return Array.from(routesByDeclName.values()).map((routes) =>
    routes.reduce((firstRoute, currentRoute) => {
      if (!areRouteDeclsEqual(currentRoute, firstRoute)) {
        throw new SpecUserError(
          `Conflicting configurations for the route \`${firstRoute.declName}\`:\n` +
            `- Definition A: ${JSON.stringify(firstRoute.declValue)}\n` +
            `- Definition B: ${JSON.stringify(currentRoute.declValue)}\n\n` +
            "All routes with the same name must produce the same configuration.",
        );
      }
      return firstRoute;
    }),
  );
}

function areRouteDeclsEqual(
  route1: AppSpec.GetDeclForType<"Route">,
  route2: AppSpec.GetDeclForType<"Route">,
): boolean {
  return (
    route1.declName === route2.declName &&
    JSON.stringify(route1.declValue) === JSON.stringify(route2.declValue)
  );
}

/**
 * Collects every {@link WaspSpec.Route} referenced by `auth` so the caller can
 * auto-register them, the same way a route auto-registers the page passed to
 * its constructor.
 */
function collectAuthRoutes(auth: WaspSpec.Auth): WaspSpec.Route[] {
  return [
    auth.onAuthFailedRedirectTo,
    auth.onAuthSucceededRedirectTo,
    auth.methods.email?.emailVerification.clientRoute,
    auth.methods.email?.passwordReset.clientRoute,
  ].filter((route) => route !== undefined);
}

export function mapQuery(
  query: WaspSpec.Query,
  ctx: AppMapperContext,
): AppSpec.Query {
  const { fn, entities, auth } = query;
  return {
    fn: ctx.mapRefObject(fn),
    entities: entities?.map(ctx.entityRefParser),
    auth,
  };
}

export function mapAction(
  action: WaspSpec.Action,
  ctx: AppMapperContext,
): AppSpec.Action {
  const { fn, entities, auth } = action;
  return {
    fn: ctx.mapRefObject(fn),
    entities: entities?.map(ctx.entityRefParser),
    auth,
  };
}

export function mapAuth(
  auth: WaspSpec.Auth,
  ctx: AppMapperContext,
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
    userEntity: ctx.entityRefParser(userEntity),
    methods: mapAuthMethods(methods, ctx),
    onAuthFailedRedirectTo: onAuthFailedRedirectTo.path,
    onAuthSucceededRedirectTo: onAuthSucceededRedirectTo?.path,
    onBeforeSignup: onBeforeSignup && ctx.mapRefObject(onBeforeSignup),
    onAfterSignup: onAfterSignup && ctx.mapRefObject(onAfterSignup),
    onAfterEmailVerified:
      onAfterEmailVerified && ctx.mapRefObject(onAfterEmailVerified),
    onBeforeOAuthRedirect:
      onBeforeOAuthRedirect && ctx.mapRefObject(onBeforeOAuthRedirect),
    onBeforeLogin: onBeforeLogin && ctx.mapRefObject(onBeforeLogin),
    onAfterLogin: onAfterLogin && ctx.mapRefObject(onAfterLogin),
  };
}

export function mapAuthMethods(
  methods: WaspSpec.AuthMethods,
  ctx: AppMapperContext,
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
      usernameAndPassword && mapUsernameAndPassword(usernameAndPassword, ctx),
    slack: slack && mapSocialAuth(slack, ctx),
    discord: discord && mapSocialAuth(discord, ctx),
    google: google && mapSocialAuth(google, ctx),
    gitHub: gitHub && mapSocialAuth(gitHub, ctx),
    keycloak: keycloak && mapSocialAuth(keycloak, ctx),
    microsoft: microsoft && mapSocialAuth(microsoft, ctx),
    email: email && mapEmailAuth(email, ctx),
  };
}

export function mapUsernameAndPassword(
  usernameAndPassword: WaspSpec.UsernameAndPasswordConfig,
  ctx: AppMapperContext,
): AppSpec.UsernameAndPasswordConfig {
  const { userSignupFields } = usernameAndPassword;
  return {
    userSignupFields: userSignupFields && ctx.mapRefObject(userSignupFields),
  };
}

export function mapSocialAuth(
  socialAuth: WaspSpec.SocialAuthConfig,
  ctx: AppMapperContext,
): AppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = socialAuth;
  return {
    configFn: configFn && ctx.mapRefObject(configFn),
    userSignupFields: userSignupFields && ctx.mapRefObject(userSignupFields),
  };
}

export function mapEmailAuth(
  emailAuth: WaspSpec.EmailAuthConfig,
  ctx: AppMapperContext,
): AppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    emailAuth;
  return {
    userSignupFields: userSignupFields && ctx.mapRefObject(userSignupFields),
    fromField: mapEmailFromField(fromField),
    emailVerification: mapEmailFlow(emailVerification, ctx),
    passwordReset: mapEmailFlow(passwordReset, ctx),
  };
}

export function mapEmailFlow(
  emailFlow: WaspSpec.EmailFlowConfig,
  ctx: AppMapperContext,
): AppSpec.EmailVerificationConfig {
  const { getEmailContentFn, clientRoute } = emailFlow;
  return {
    getEmailContentFn: getEmailContentFn && ctx.mapRefObject(getEmailContentFn),
    clientRoute: ctx.routeRefParser(clientRoute.name),
  };
}

export function mapApi(api: WaspSpec.Api, ctx: AppMapperContext): AppSpec.Api {
  const { method, path, fn, middlewareConfigFn, entities, auth } = api;
  return {
    fn: ctx.mapRefObject(fn),
    middlewareConfigFn:
      middlewareConfigFn && ctx.mapRefObject(middlewareConfigFn),
    entities: entities?.map(ctx.entityRefParser),
    httpRoute: [method, path],
    auth,
  };
}

export function mapApiNamespace(
  apiNamespace: WaspSpec.ApiNamespace,
  ctx: AppMapperContext,
): AppSpec.ApiNamespace {
  const { middlewareConfigFn, path } = apiNamespace;
  return {
    middlewareConfigFn: ctx.mapRefObject(middlewareConfigFn),
    path,
  };
}

export function mapServer(
  server: WaspSpec.Server,
  ctx: AppMapperContext,
): AppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server;
  return {
    setupFn: setupFn && ctx.mapRefObject(setupFn),
    middlewareConfigFn:
      middlewareConfigFn && ctx.mapRefObject(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && ctx.mapRefObject(envValidationSchema),
  };
}

export function mapClient(
  client: WaspSpec.Client,
  ctx: AppMapperContext,
): AppSpec.Client {
  const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
  return {
    rootComponent: rootComponent && ctx.mapRefObject(rootComponent),
    setupFn: setupFn && ctx.mapRefObject(setupFn),
    baseDir,
    envValidationSchema:
      envValidationSchema && ctx.mapRefObject(envValidationSchema),
  };
}

export function mapDb(db: WaspSpec.Db, ctx: AppMapperContext): AppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(ctx.mapRefObject),
    prismaSetupFn: prismaSetupFn && ctx.mapRefObject(prismaSetupFn),
  };
}

export function mapEmailSender(
  emailSender: WaspSpec.EmailSender,
): AppSpec.EmailSender {
  const { provider, defaultFrom } = emailSender;
  return {
    provider,
    defaultFrom: defaultFrom && mapEmailFromField(defaultFrom),
  };
}

export function mapEmailFromField(
  emailFromField: WaspSpec.EmailFromField,
): AppSpec.EmailFromField {
  return {
    name: emailFromField.name,
    email: emailFromField.email,
  };
}

export function mapWebSocket(
  webSocket: WaspSpec.WebSocket,
  ctx: AppMapperContext,
): AppSpec.WebSocket {
  const { fn, autoConnect } = webSocket;
  return {
    fn: ctx.mapRefObject(fn),
    autoConnect,
  };
}

export function mapJob(job: WaspSpec.Job, ctx: AppMapperContext): AppSpec.Job {
  const { fn, executor, schedule, entities, performExecutorOptions } = job;
  return {
    executor,
    perform: {
      fn: ctx.mapRefObject(fn),
      executorOptions: performExecutorOptions,
    },
    schedule: schedule && mapSchedule(schedule),
    entities: entities?.map(ctx.entityRefParser),
  };
}

export function mapCrud(
  crud: WaspSpec.Crud,
  ctx: AppMapperContext,
): AppSpec.Crud {
  const { entity, operations } = crud;
  return {
    entity: ctx.entityRefParser(entity),
    operations: mapCrudOperations(operations, ctx),
  };
}

export function mapCrudOperations(
  operations: WaspSpec.CrudOperations,
  ctx: AppMapperContext,
): AppSpec.CrudOperations {
  const { get, getAll, create, update, delete: del } = operations;
  return {
    get: get && mapCrudOperationOptions(get, ctx),
    getAll: getAll && mapCrudOperationOptions(getAll, ctx),
    create: create && mapCrudOperationOptions(create, ctx),
    update: update && mapCrudOperationOptions(update, ctx),
    delete: del && mapCrudOperationOptions(del, ctx),
  };
}

export function mapCrudOperationOptions(
  options: WaspSpec.CrudOperationOptions,
  ctx: AppMapperContext,
): AppSpec.CrudOperationOptions {
  const { isPublic, overrideFn } = options;
  return {
    isPublic,
    overrideFn: overrideFn && ctx.mapRefObject(overrideFn),
  };
}

export function mapSchedule(schedule: WaspSpec.Schedule): AppSpec.Schedule {
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

function extractSpecElements<Kind extends WaspSpec.SpecElement["kind"]>(
  id: Kind,
  spec: WaspSpec.SpecElement[],
): GetSpecElementForKind<Kind>[] {
  return spec.filter((p): p is GetSpecElementForKind<Kind> => p.kind === id);
}

type GetSpecElementForKind<Kind extends WaspSpec.SpecElement["kind"]> = Extract<
  WaspSpec.SpecElement,
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

function flattenSpec(spec: WaspSpec.Spec): WaspSpec.SpecElement[] {
  // We assert the `[spec]` as a `SpecElement[]` to avoid
  // inifnite recursion of the `WaspSpec.Spec` type.
  return ([spec] as WaspSpec.SpecElement[]).flat(Infinity);
}

/**
 * The point of this function is to enforce exhaustivness over all AppSpec
 * declaration types, ensuring we don't forget to include anything.
 * Check the original comment for details: https://github.com/wasp-lang/wasp/pull/2393#discussion_r1866620833
 *
 * TODO: The new spec bundles all specifications (queries, actions...) together in
 * the `spec` array, so there's no need to go through them one by one.
 *
 * We'd likely be better off by:
 *   1. Mapping the entire array with a dispatcher that calls the correct
 *   mapper depending on the declaration's kind
 *   2. Passing this mapped array into the app spec (which expects them all on
 *   the same level anyway).
 * We'll likely lose some mapping type safety in the process though. Explore
 * when we're done with the port from legacy to the new spec.
 */
function ensureAllDecls(decls: {
  [Type in AppSpec.Decl["declType"]]: AppSpec.GetDeclForType<Type>[];
}): AppSpec.Decl[] {
  return Object.values(decls).flat();
}
