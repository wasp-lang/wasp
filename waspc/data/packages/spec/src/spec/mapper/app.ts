/**
 * @module
 *
 * This module maps the Wasp Spec to the internal representation of
 * the app ({@link AppSpec.Decl}).
 * All of the mapping functions are exported so that they can be individually
 * tested.
 */

import { isEqual } from "es-toolkit";
import { mapRefObject } from "../refObject.js";
import { SpecUserError } from "../specUserError.js";
import {
  AppSpecDeclTypeForWaspSpecElement,
  mapWaspSpecElement,
} from "./specElements.js";
import type { AppMapperContext, AppSpec, WaspSpec } from "./types.js";

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

  // Keyed by `declType:declName`: decl names only have to be unique within
  // their decl type (mirroring waspc's AppSpec validation), so decls of
  // different types may share a name.
  const specElementDecls = new Map<string, AppSpec.Decl>();

  const ctx: AppMapperContext = {
    emitEntityRef: makeRefParser("Entity", entityNames),

    emitRefObject: (refObject: unknown) =>
      mapRefObject(refObject, {
        projectRootDir,
      }),

    emitSpecElementRef: <SpecElement extends WaspSpec.SpecElement>(
      specElement: SpecElement,
    ) => {
      const decl = mapWaspSpecElement(specElement, ctx);

      const declKey = `${decl.declType}:${decl.declName}`;
      const oldDecl = specElementDecls.get(declKey);
      if (oldDecl && !isEqual(oldDecl, decl)) {
        throw makeConflictingDeclsError(oldDecl, decl);
      }

      specElementDecls.set(declKey, decl);

      return { declType: decl.declType, name: decl.declName } as AppSpec.Ref<
        AppSpecDeclTypeForWaspSpecElement<SpecElement>
      >;
    },
  };

  for (const specElement of flattenSpec(spec)) {
    ctx.emitSpecElementRef(specElement);
  }

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

  return [appDecl, ...specElementDecls.values()];
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

  // `auth` auto-registers the destinations it references, the same way a
  // route auto-registers the page passed to its constructor.
  ctx.emitSpecElementRef(onAuthFailedRedirectTo);
  if (onAuthSucceededRedirectTo) {
    ctx.emitSpecElementRef(onAuthSucceededRedirectTo);
  }

  return {
    userEntity: ctx.emitEntityRef(userEntity),
    methods: mapAuthMethods(methods, ctx),
    onAuthFailedRedirectTo: onAuthFailedRedirectTo.path,
    onAuthSucceededRedirectTo: onAuthSucceededRedirectTo?.path,
    onBeforeSignup: onBeforeSignup && ctx.emitRefObject(onBeforeSignup),
    onAfterSignup: onAfterSignup && ctx.emitRefObject(onAfterSignup),
    onAfterEmailVerified:
      onAfterEmailVerified && ctx.emitRefObject(onAfterEmailVerified),
    onBeforeOAuthRedirect:
      onBeforeOAuthRedirect && ctx.emitRefObject(onBeforeOAuthRedirect),
    onBeforeLogin: onBeforeLogin && ctx.emitRefObject(onBeforeLogin),
    onAfterLogin: onAfterLogin && ctx.emitRefObject(onAfterLogin),
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
    userSignupFields: userSignupFields && ctx.emitRefObject(userSignupFields),
  };
}

export function mapSocialAuth(
  socialAuth: WaspSpec.SocialAuthConfig,
  ctx: AppMapperContext,
): AppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = socialAuth;
  return {
    configFn: configFn && ctx.emitRefObject(configFn),
    userSignupFields: userSignupFields && ctx.emitRefObject(userSignupFields),
  };
}

export function mapEmailAuth(
  emailAuth: WaspSpec.EmailAuthConfig,
  ctx: AppMapperContext,
): AppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    emailAuth;
  return {
    userSignupFields: userSignupFields && ctx.emitRefObject(userSignupFields),
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

  ctx.emitSpecElementRef(clientRoute);

  return {
    getEmailContentFn:
      getEmailContentFn && ctx.emitRefObject(getEmailContentFn),
    clientRoute: clientRoute.path,
  };
}

export function mapServer(
  server: WaspSpec.Server,
  ctx: AppMapperContext,
): AppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server;
  return {
    setupFn: setupFn && ctx.emitRefObject(setupFn),
    middlewareConfigFn:
      middlewareConfigFn && ctx.emitRefObject(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && ctx.emitRefObject(envValidationSchema),
  };
}

export function mapClient(
  client: WaspSpec.Client,
  ctx: AppMapperContext,
): AppSpec.Client {
  const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
  return {
    rootComponent: rootComponent && ctx.emitRefObject(rootComponent),
    setupFn: setupFn && ctx.emitRefObject(setupFn),
    baseDir,
    envValidationSchema:
      envValidationSchema && ctx.emitRefObject(envValidationSchema),
  };
}

export function mapDb(db: WaspSpec.Db, ctx: AppMapperContext): AppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(ctx.emitRefObject),
    prismaSetupFn: prismaSetupFn && ctx.emitRefObject(prismaSetupFn),
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
    fn: ctx.emitRefObject(fn),
    autoConnect,
  };
}

export function makeRefParser<T extends AppSpec.DeclType>(
  declType: T,
  declNames: string[],
): (name: string) => AppSpec.Ref<T> {
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

function flattenSpec(spec: WaspSpec.Spec): WaspSpec.SpecElement[] {
  // We assert the `[spec]` as a `SpecElement[]` to avoid
  // infinite recursion of the `WaspSpec.Spec` type.
  return ([spec] as WaspSpec.SpecElement[]).flat(Infinity);
}

const declTypeDisplayNames = {
  App: "app",
  Page: "page",
  Route: "route",
  Query: "query",
  Action: "action",
  Api: "API",
  ApiNamespace: "API namespace",
  Job: "job",
  Crud: "CRUD",
} as const satisfies Record<AppSpec.Decl["declType"], string>;

function makeConflictingDeclsError(
  existingDecl: AppSpec.Decl,
  incomingDecl: AppSpec.Decl,
): SpecUserError {
  return new SpecUserError(
    `Conflicting configurations for the ${declTypeDisplayNames[existingDecl.declType]} \`${existingDecl.declName}\`:\n` +
      `- Definition A: ${JSON.stringify(existingDecl.declValue)}\n` +
      `- Definition B: ${JSON.stringify(incomingDecl.declValue)}\n\n` +
      `All definitions with the same name must produce the same configuration.\n` +
      "If the duplication was intentional, please use a different name to differentiate them.",
  );
}
