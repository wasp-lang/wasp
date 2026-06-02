/**
 * This module maps the TsAppSpec-facing API to the internal representation of
 * All of the mapping functions are exported so that they can be individually
 * tested.
 * the app ({@link OutputAppSpec.Decl}).
 */

import * as OutputAppSpec from "../appSpec.js";
import * as InputAppSpec from "./publicApi/tsAppSpec.js";
import { getRefObjectDeclarationName, mapRefObject } from "./refObject.js";
import { SpecUserError } from "./specUserError.js";

export function mapApp(
  appInput: InputAppSpec.App,
  {
    projectRootDir,
    entityNames,
  }: {
    projectRootDir: string;
    entityNames: string[];
  },
): OutputAppSpec.Decl[] {
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
    decls,
  } = appInput;

  const entityRefParser = makeRefParser("Entity", entityNames);
  const routesInput = extractInputDecls("route", decls);
  const routeRefParser = makeRefParser(
    "Route",
    routesInput.map((r) => r.name),
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
  // (e.g., maybe named parameters, maybe putting extractInputDecls inside
  // mapDecls)
  const pagesInput = extractInputDecls("page", decls);
  const pagesOutput = mapDecls(
    pagesInput,
    "Page",
    (page) => getRefObjectDeclarationName(page.component),
    (page) => mapPage(page, ctx),
  );

  const routesOutput = mapDecls(
    routesInput,
    "Route",
    (route) => route.name,
    (route) => mapRoute(route),
  );
  const routePagesOutputs = mapDecls(
    routesInput,
    "Page",
    (route) => getRefObjectDeclarationName(route.page.component),
    (route) => mapPage(route.page, ctx),
  );

  const queriesInput = extractInputDecls("query", decls);
  const queriesOutput = mapDecls(
    queriesInput,
    "Query",
    (query) => getRefObjectDeclarationName(query.fn),
    (query) => mapQuery(query, ctx),
  );

  const actionsInput = extractInputDecls("action", decls);
  const actionsOutput = mapDecls(
    actionsInput,
    "Action",
    (action) => getRefObjectDeclarationName(action.fn),
    (action) => mapAction(action, ctx),
  );

  const apisInput = extractInputDecls("api", decls);
  const apisOutput = mapDecls(
    apisInput,
    "Api",
    (api) => getRefObjectDeclarationName(api.fn),
    (api) => mapApi(api, ctx),
  );

  const apiNamespacesInput = extractInputDecls("apiNamespace", decls);
  const apiNamespacesOutput = mapDecls(
    apiNamespacesInput,
    "ApiNamespace",
    (ns) => getRefObjectDeclarationName(ns.middlewareConfigFn),
    (ns) => mapApiNamespace(ns, ctx),
  );

  const jobsInput = extractInputDecls("job", decls);
  const jobsOutput = mapDecls(
    jobsInput,
    "Job",
    (job) => getRefObjectDeclarationName(job.fn),
    (job) => mapJob(job, ctx),
  );

  const crudsInput = extractInputDecls("crud", decls);
  const crudsOutput = mapDecls(
    crudsInput,
    "Crud",
    (crud) => crud.name,
    (crud) => mapCrud(crud, ctx),
  );

  const appOutput = {
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

  return ensureAllOutputDecls({
    App: [appOutput],
    Page: dedupePageDecls([...pagesOutput, ...routePagesOutputs]),
    Route: routesOutput,
    Query: queriesOutput,
    Action: actionsOutput,
    Api: apisOutput,
    ApiNamespace: apiNamespacesOutput,
    Job: jobsOutput,
    Crud: crudsOutput,
  });
}

export type AppMapperContext = {
  entityRefParser: RefParser<"Entity">;
  routeRefParser: RefParser<"Route">;
  mapRefObject: RefObjectMapper;
};

type RefObjectMapper = (refObject: unknown) => OutputAppSpec.ExtImport;

export function mapPage(
  page: InputAppSpec.Page,
  ctx: AppMapperContext,
): OutputAppSpec.Page {
  const { component, authRequired } = page;
  return {
    component: ctx.mapRefObject(component),
    authRequired,
  };
}

export function mapRoute(route: InputAppSpec.Route): OutputAppSpec.Route {
  const { path, prerender, lazy } = route;
  return {
    path,
    to: {
      name: getRefObjectDeclarationName(route.page.component),
      declType: "Page",
    },
    prerender,
    lazy,
  };
}

/**
 * {@link InputAppSpec.Route} through it's constructor can either:
 * - Create a new {@link InputAppSpec.Page}.
 * - Reference an existing {@link InputAppSpec.Page}.
 *
 * In case when it references an existing page, we don't want to
 * count the reference as a separate {@link OutputAppSpec.Page} declaration.
 */
export function dedupePageDecls(
  decls: OutputAppSpec.GetDeclForType<"Page">[],
): OutputAppSpec.GetDeclForType<"Page">[] {
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
  page1: OutputAppSpec.GetDeclForType<"Page">,
  page2: OutputAppSpec.GetDeclForType<"Page">,
): boolean {
  const isSameCanonicalPage = page1.declName === page2.declName;
  return (
    isSameCanonicalPage &&
    JSON.stringify(page1.declValue) === JSON.stringify(page2.declValue)
  );
}

export function mapQuery(
  query: InputAppSpec.Query,
  ctx: AppMapperContext,
): OutputAppSpec.Query {
  const { fn, entities, auth } = query;
  return {
    fn: ctx.mapRefObject(fn),
    entities: entities?.map(ctx.entityRefParser),
    auth,
  };
}

export function mapAction(
  action: InputAppSpec.Action,
  ctx: AppMapperContext,
): OutputAppSpec.Action {
  const { fn, entities, auth } = action;
  return {
    fn: ctx.mapRefObject(fn),
    entities: entities?.map(ctx.entityRefParser),
    auth,
  };
}

export function mapAuth(
  auth: InputAppSpec.Auth,
  ctx: AppMapperContext,
): OutputAppSpec.Auth {
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
    onAuthFailedRedirectTo,
    onAuthSucceededRedirectTo,
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
  methods: InputAppSpec.AuthMethods,
  ctx: AppMapperContext,
): OutputAppSpec.AuthMethods {
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
  usernameAndPassword: InputAppSpec.UsernameAndPasswordConfig,
  ctx: AppMapperContext,
): OutputAppSpec.UsernameAndPasswordConfig {
  const { userSignupFields } = usernameAndPassword;
  return {
    userSignupFields: userSignupFields && ctx.mapRefObject(userSignupFields),
  };
}

export function mapSocialAuth(
  socialAuth: InputAppSpec.SocialAuthConfig,
  ctx: AppMapperContext,
): OutputAppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = socialAuth;
  return {
    configFn: configFn && ctx.mapRefObject(configFn),
    userSignupFields: userSignupFields && ctx.mapRefObject(userSignupFields),
  };
}

export function mapEmailAuth(
  emailAuth: InputAppSpec.EmailAuthConfig,
  ctx: AppMapperContext,
): OutputAppSpec.EmailAuthConfig {
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
  emailFlow: InputAppSpec.EmailFlowConfig,
  ctx: AppMapperContext,
): OutputAppSpec.EmailVerificationConfig {
  const { getEmailContentFn, clientRoute } = emailFlow;
  return {
    getEmailContentFn: getEmailContentFn && ctx.mapRefObject(getEmailContentFn),
    clientRoute: ctx.routeRefParser(clientRoute),
  };
}

export function mapApi(
  api: InputAppSpec.Api,
  ctx: AppMapperContext,
): OutputAppSpec.Api {
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
  apiNamespace: InputAppSpec.ApiNamespace,
  ctx: AppMapperContext,
): OutputAppSpec.ApiNamespace {
  const { middlewareConfigFn, path } = apiNamespace;
  return {
    middlewareConfigFn: ctx.mapRefObject(middlewareConfigFn),
    path,
  };
}

export function mapServer(
  server: InputAppSpec.Server,
  ctx: AppMapperContext,
): OutputAppSpec.Server {
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
  client: InputAppSpec.Client,
  ctx: AppMapperContext,
): OutputAppSpec.Client {
  const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
  return {
    rootComponent: rootComponent && ctx.mapRefObject(rootComponent),
    setupFn: setupFn && ctx.mapRefObject(setupFn),
    baseDir,
    envValidationSchema:
      envValidationSchema && ctx.mapRefObject(envValidationSchema),
  };
}

export function mapDb(
  db: InputAppSpec.Db,
  ctx: AppMapperContext,
): OutputAppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(ctx.mapRefObject),
    prismaSetupFn: prismaSetupFn && ctx.mapRefObject(prismaSetupFn),
  };
}

export function mapEmailSender(
  emailSender: InputAppSpec.EmailSender,
): OutputAppSpec.EmailSender {
  const { provider, defaultFrom } = emailSender;
  return {
    provider,
    defaultFrom: defaultFrom && mapEmailFromField(defaultFrom),
  };
}

export function mapEmailFromField(
  emailFromField: InputAppSpec.EmailFromField,
): OutputAppSpec.EmailFromField {
  return {
    name: emailFromField.name,
    email: emailFromField.email,
  };
}

export function mapWebSocket(
  webSocket: InputAppSpec.WebSocket,
  ctx: AppMapperContext,
): OutputAppSpec.WebSocket {
  const { fn, autoConnect } = webSocket;
  return {
    fn: ctx.mapRefObject(fn),
    autoConnect,
  };
}

export function mapJob(
  job: InputAppSpec.Job,
  ctx: AppMapperContext,
): OutputAppSpec.Job {
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
  crud: InputAppSpec.Crud,
  ctx: AppMapperContext,
): OutputAppSpec.Crud {
  const { entity, operations } = crud;
  return {
    entity: ctx.entityRefParser(entity),
    operations: mapCrudOperations(operations, ctx),
  };
}

export function mapCrudOperations(
  operations: InputAppSpec.CrudOperations,
  ctx: AppMapperContext,
): OutputAppSpec.CrudOperations {
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
  options: InputAppSpec.CrudOperationOptions,
  ctx: AppMapperContext,
): OutputAppSpec.CrudOperationOptions {
  const { isPublic, overrideFn } = options;
  return {
    isPublic,
    overrideFn: overrideFn && ctx.mapRefObject(overrideFn),
  };
}

export function mapSchedule(
  schedule: InputAppSpec.Schedule,
): OutputAppSpec.Schedule {
  const { cron, args, executorOptions } = schedule;
  return {
    cron,
    args,
    executorOptions,
  };
}

export type RefParser<T extends OutputAppSpec.DeclType> = (
  name: string,
) => OutputAppSpec.Ref<T>;

export function makeRefParser<T extends OutputAppSpec.DeclType>(
  declType: T,
  declNames: string[],
): RefParser<T> {
  return function parseRef(potentialRef: string): OutputAppSpec.Ref<T> {
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

function extractInputDecls<Kind extends InputAppSpec.Decl["kind"]>(
  id: Kind,
  decls: InputAppSpec.Decl[],
): GetInputDeclForKind<Kind>[] {
  return decls.filter((p): p is GetInputDeclForKind<Kind> => p.kind === id);
}

type GetInputDeclForKind<Kind extends InputAppSpec.Decl["kind"]> = Extract<
  InputAppSpec.Decl,
  { kind: Kind }
>;

function mapDecls<T, DeclType extends OutputAppSpec.Decl["declType"]>(
  items: T[],
  declType: DeclType,
  deriveName: (item: T) => string,
  mapValue: (item: T) => OutputAppSpec.GetDeclForType<DeclType>["declValue"],
) {
  return items.map((item) => ({
    declType,
    declName: deriveName(item),
    declValue: mapValue(item),
  }));
}

/**
 * The point of this function is to enforce exhaustivness over all declaration
 * types, ensuring we don't forget to include anything.
 * Check the original comment for details: https://github.com/wasp-lang/wasp/pull/2393#discussion_r1866620833
 *
 * TODO: The new spec bundles all declarations (queries, actions...) together in
 * the decls array, so there's no need to go through them one by one.
 *
 * We'd likely be better off by:
 *   1. Mapping the entire array with a dispatcher that calls the correct
 *   mapper depending on the declaration's kind
 *   2. Passing this mapped array into the app spec (which expects them all on
 *   the same level anyway).
 * We'll likely lose some mapping type safety in the process though. Explore
 * when we're done with the port from legacy to the new spec.
 */
function ensureAllOutputDecls(decls: {
  [Type in OutputAppSpec.Decl["declType"]]: OutputAppSpec.GetDeclForType<Type>[];
}): OutputAppSpec.Decl[] {
  return Object.values(decls).flat();
}
