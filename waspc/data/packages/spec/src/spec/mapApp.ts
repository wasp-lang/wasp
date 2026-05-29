/**
 * This module maps the TsAppSpec-facing API to the internal representation of
 * the app ({@link OutputAppSpec.Decl})
 * All of the mapping functions are exported so that they can be individually
 * tested.
 */

import * as OutputAppSpec from "../appSpec.js";
import type { AnyFunction } from "../typeUtils.js";
import { mapExtImport } from "./refImport.js";
import * as InputAppSpec from "./publicApi/tsAppSpec.js";
import { SpecUserError } from "./specUserError.js";

export function mapApp(
  appInput: InputAppSpec.App,
  entityNames: string[],
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

  // TODO: When you add all declarations, see if you can generalize better
  // (e.g., maybe named parameters, maybe putting extractDecls inside
  // mapToDecls)
  const pagesInput = extractInputDecls("page", decls);
  const pagesOutput = mapDecls(
    pagesInput,
    "Page",
    (page) => deriveExtImportName(page.component),
    mapPage,
  );

  const routesInput = extractInputDecls("route", decls);
  const routesOutput = mapDecls(
    routesInput,
    "Route",
    (route) => route.name,
    (route) => mapRoute(route),
  );
  const routePagesOutputs = mapDecls(
    routesInput,
    "Page",
    (route) => deriveExtImportName(route.page.component),
    (route) => mapPage(route.page),
  );
  const routeRefParser = makeRefParser(
    "Route",
    routesInput.map((r) => r.name),
  );

  const queriesInput = extractInputDecls("query", decls);
  const queriesOutput = mapDecls(
    queriesInput,
    "Query",
    (query) => deriveExtImportName(query.fn),
    (query) => mapQuery(query, entityRefParser),
  );

  const actionsInput = extractInputDecls("action", decls);
  const actionsOutput = mapDecls(
    actionsInput,
    "Action",
    (action) => deriveExtImportName(action.fn),
    (action) => mapAction(action, entityRefParser),
  );

  const apisInput = extractInputDecls("api", decls);
  const apisOutput = mapDecls(
    apisInput,
    "Api",
    (api) => deriveExtImportName(api.fn),
    (api) => mapApi(api, entityRefParser),
  );

  const apiNamespacesInput = extractInputDecls("apiNamespace", decls);
  const apiNamespacesOutput = mapDecls(
    apiNamespacesInput,
    "ApiNamespace",
    (ns) => deriveExtImportName(ns.middlewareConfigFn),
    mapApiNamespace,
  );

  const jobsInput = extractInputDecls("job", decls);
  const jobsOutput = mapDecls(
    jobsInput,
    "Job",
    (job) => deriveExtImportName(job.fn),
    (job) => mapJob(job, entityRefParser),
  );

  const crudsInput = extractInputDecls("crud", decls);
  const crudsOutput = mapDecls(
    crudsInput,
    "Crud",
    (crud) => crud.name,
    (crud) => mapCrud(crud, entityRefParser),
  );

  const appOutput = {
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

export function mapPage(page: InputAppSpec.Page): OutputAppSpec.Page {
  const { component, authRequired } = page;
  return {
    component: mapExtImport(component),
    authRequired,
  };
}

export function mapRoute(route: InputAppSpec.Route): OutputAppSpec.Route {
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
  entityRefParser: RefParser<"Entity">,
): OutputAppSpec.Query {
  const { fn, entities, auth } = query;
  return {
    fn: mapExtImport(fn),
    entities: entities?.map(entityRefParser),
    auth,
  };
}

export function mapAction(
  action: InputAppSpec.Action,
  entityRefParser: RefParser<"Entity">,
): OutputAppSpec.Action {
  const { fn, entities, auth } = action;
  return {
    fn: mapExtImport(fn),
    entities: entities?.map(entityRefParser),
    auth,
  };
}

export function mapAuth(
  auth: InputAppSpec.Auth,
  entityRefParser: RefParser<"Entity">,
  routeRefParser: RefParser<"Route">,
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
    userEntity: entityRefParser(userEntity),
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
  methods: InputAppSpec.AuthMethods,
  routeRefParser: RefParser<"Route">,
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
  usernameAndPassword: InputAppSpec.UsernameAndPasswordConfig,
): OutputAppSpec.UsernameAndPasswordConfig {
  const { userSignupFields } = usernameAndPassword;
  return {
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
  };
}

export function mapSocialAuth(
  socialAuth: InputAppSpec.SocialAuthConfig,
): OutputAppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = socialAuth;
  return {
    configFn: configFn && mapExtImport(configFn),
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
  };
}

export function mapEmailAuth(
  emailAuth: InputAppSpec.EmailAuthConfig,
  routeRefParser: RefParser<"Route">,
): OutputAppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    emailAuth;
  return {
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
    fromField: mapEmailFromField(fromField),
    emailVerification: mapEmailFlow(emailVerification, routeRefParser),
    passwordReset: mapEmailFlow(passwordReset, routeRefParser),
  };
}

export function mapEmailFlow(
  emailFlow: InputAppSpec.EmailFlowConfig,
  routeRefParser: RefParser<"Route">,
): OutputAppSpec.EmailVerificationConfig {
  const { getEmailContentFn, clientRoute } = emailFlow;
  return {
    getEmailContentFn: getEmailContentFn && mapExtImport(getEmailContentFn),
    clientRoute: routeRefParser(clientRoute),
  };
}

export function mapApi(
  api: InputAppSpec.Api,
  entityRefParser: RefParser<"Entity">,
): OutputAppSpec.Api {
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
  apiNamespace: InputAppSpec.ApiNamespace,
): OutputAppSpec.ApiNamespace {
  const { middlewareConfigFn, path } = apiNamespace;
  return {
    middlewareConfigFn: mapExtImport(middlewareConfigFn),
    path,
  };
}

export function mapServer(server: InputAppSpec.Server): OutputAppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server;
  return {
    setupFn: setupFn && mapExtImport(setupFn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  };
}

export function mapClient(client: InputAppSpec.Client): OutputAppSpec.Client {
  const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
  return {
    rootComponent: rootComponent && mapExtImport(rootComponent),
    setupFn: setupFn && mapExtImport(setupFn),
    baseDir,
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  };
}

export function mapDb(db: InputAppSpec.Db): OutputAppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(mapExtImport),
    prismaSetupFn: prismaSetupFn && mapExtImport(prismaSetupFn),
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
): OutputAppSpec.WebSocket {
  const { fn, autoConnect } = webSocket;
  return {
    fn: mapExtImport(fn),
    autoConnect,
  };
}

export function mapJob(
  job: InputAppSpec.Job,
  entityRefParser: RefParser<"Entity">,
): OutputAppSpec.Job {
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
  crud: InputAppSpec.Crud,
  entityRefParser: RefParser<"Entity">,
): OutputAppSpec.Crud {
  const { entity, operations } = crud;
  return {
    entity: entityRefParser(entity),
    operations: mapCrudOperations(operations),
  };
}

export function mapCrudOperations(
  operations: InputAppSpec.CrudOperations,
): OutputAppSpec.CrudOperations {
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
  options: InputAppSpec.CrudOperationOptions,
): OutputAppSpec.CrudOperationOptions {
  const { isPublic, overrideFn } = options;
  return {
    isPublic,
    overrideFn: overrideFn && mapExtImport(overrideFn),
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

export function deriveExtImportName(
  extImport: InputAppSpec.RefImport | InputAppSpec.ExtImport | AnyFunction,
): string {
  const mappedExtImport = mapExtImport(extImport);

  return "alias" in mappedExtImport
    ? (mappedExtImport.alias ?? mappedExtImport.name)
    : mappedExtImport.name;
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
