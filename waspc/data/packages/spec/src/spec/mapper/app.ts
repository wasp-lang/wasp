import { isEqual } from "es-toolkit";
import * as AppSpec from "../../appSpec.js";
import type { AnyObject } from "../../typeUtils.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { WaspSpecUserError } from "../waspSpecUserError.js";
import { AppMapperContext } from "./context.js";

export function mapAppSpec(
  app: WaspSpec.App,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"App"> {
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
  } = app;

  return {
    declType: "App",
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
}

export function mapAuth(
  auth: WaspSpec.Auth,
  ctx: AppMapperContext,
): AppSpec.Auth {
  const { userEntity, onAuthFailedRedirectTo, providers } = auth;

  if ("provider" in auth) {
    throw new WaspSpecUserError(
      "app.auth.provider was renamed to app.auth.providers (an array of providers). Wrap your provider in an array: providers: [waspAuth({ ... })].",
    );
  }

  if (!Array.isArray(providers) || providers.length === 0) {
    throw new WaspSpecUserError(
      "app.auth.providers must be a non-empty array of providers, each created with waspAuth(), an auth adapter package's spec helper, or customAuthProvider().",
    );
  }

  const mappedProviders = providers.map((provider) =>
    mapAuthProvider(provider, ctx),
  );
  assertProviderIdsAreUnique(mappedProviders);

  return {
    userEntity: ctx.resolveEntityRef(userEntity),
    onAuthFailedRedirectTo,
    providers: mappedProviders,
  };
}

// The user-facing union maps 1:1 onto the IR's union -- the IR is the same
// discriminated shape, so the impossible states never exist in any
// representation the compiler consumes.
function mapAuthProvider(
  provider: WaspSpec.AuthProviderConfig,
  ctx: AppMapperContext,
): AppSpec.AuthProvider {
  switch (provider.kind) {
    case "wasp": {
      const {
        methods,
        onAuthSucceededRedirectTo,
        onBeforeSignup,
        onAfterSignup,
        onAfterEmailVerified,
        onBeforeOAuthRedirect,
        onBeforeLogin,
        onAfterLogin,
      } = provider.config;

      return {
        kind: "wasp",
        methods: mapAuthMethods(methods, ctx),
        onAuthSucceededRedirectTo,
        onBeforeSignup: onBeforeSignup && ctx.parseRefObject(onBeforeSignup),
        onAfterSignup: onAfterSignup && ctx.parseRefObject(onAfterSignup),
        onAfterEmailVerified:
          onAfterEmailVerified && ctx.parseRefObject(onAfterEmailVerified),
        onBeforeOAuthRedirect:
          onBeforeOAuthRedirect && ctx.parseRefObject(onBeforeOAuthRedirect),
        onBeforeLogin: onBeforeLogin && ctx.parseRefObject(onBeforeLogin),
        onAfterLogin: onAfterLogin && ctx.parseRefObject(onAfterLogin),
      };
    }
    case "external":
      return {
        kind: "external",
        ...mapExternalAuthProvider(provider, ctx),
      };
    default:
      throw new WaspSpecUserError(
        "Each entry of app.auth.providers must be created with waspAuth(), an auth adapter package's spec helper, or customAuthProvider().",
      );
  }
}

// Identities (and sessions) are recorded under the provider id, so a
// duplicate would silently merge two providers' subjects. Checked here so the
// error carries the offending id; the Haskell validator mirrors it.
function assertProviderIdsAreUnique(providers: AppSpec.AuthProvider[]): void {
  const seenIds = new Set<string>();
  for (const provider of providers) {
    const providerId = provider.kind === "wasp" ? "wasp" : provider.providerId;
    if (seenIds.has(providerId)) {
      throw new WaspSpecUserError(
        providerId === "wasp"
          ? "app.auth.providers may contain at most one waspAuth(...) provider."
          : `app.auth.providers contains provider id '${providerId}' more than once. Identities are recorded under this id, so each provider may appear at most once (provider instance ids are not configurable yet).`,
      );
    }
    seenIds.add(providerId);
  }
}

function mapExternalAuthProvider(
  manifest: WaspSpec.ExternalAuthProviderManifest,
  ctx: AppMapperContext,
): AppSpec.ExternalAuthProviderSpec {
  if (manifest.__waspAuthProviderManifest !== true) {
    throw new WaspSpecUserError(
      "app.auth.providers received a hand-crafted external provider manifest. Manifests must be created through an adapter package's spec helper or customAuthProvider(), so they go through Wasp's validation.",
    );
  }

  if (manifest.contractVersion !== 1) {
    throw new WaspSpecUserError(
      `Auth provider '${manifest.id}' was built against auth contract version ${String(
        manifest.contractVersion,
      )}, but this version of Wasp only supports version 1. Update Wasp, or use an adapter version matching your Wasp version.`,
    );
  }

  // Reserved for a future in which adapter packages contribute Prisma models.
  // Erroring (rather than ignoring) means an adapter relying on them can never
  // appear to work while its models silently don't exist.
  for (const reservedField of ["prismaModels", "manageSchema"]) {
    if (reservedField in manifest) {
      throw new WaspSpecUserError(
        `Auth provider '${manifest.id}' sets '${reservedField}', which this version of Wasp does not support yet.`,
      );
    }
  }

  const isPackageEntry = "package" in manifest.server;

  return {
    providerId: manifest.id,
    server: isPackageEntry
      ? { package: (manifest.server as { package: string }).package }
      : {
          module: ctx.parseRefObject(
            manifest.server as WaspSpec.Reference<AnyObject>,
          ),
        },
    clientPackage: manifest.client?.package,
    routes: manifest.routes && {
      basePath: manifest.routes.basePath,
      rawBody: manifest.routes.rawBody,
    },
    capabilities: manifest.capabilities,
    envVars: {
      server: manifest.env.server.map(mapEnvVarRequirement),
      client: manifest.env.client.map(mapEnvVarRequirement),
    },
    userSignupFields:
      manifest.userSignupFields &&
      ctx.parseRefObject(manifest.userSignupFields),
    setupFn: manifest.setupFn && ctx.parseRefObject(manifest.setupFn),
    optionsJson: mapProviderOptions(manifest),
  };
}

function mapEnvVarRequirement(
  envVar: WaspSpec.EnvVarRequirement,
): AppSpec.ExternalProviderEnvVar {
  return { name: envVar.name, optional: envVar.optional, doc: envVar.doc };
}

function mapProviderOptions(
  manifest: WaspSpec.ExternalAuthProviderManifest,
): string | undefined {
  if (manifest.options === undefined) {
    return undefined;
  }

  // Options travel to the generated code as JSON, so anything that doesn't
  // survive the round-trip (functions, class instances, undefined-holed
  // arrays) would arrive silently mangled. Rejecting here turns that into an
  // error at compile time, with `setupFn` as the documented escape
  // hatch for non-serializable configuration.
  const optionsJson = JSON.stringify(manifest.options);
  if (
    optionsJson === undefined ||
    !isEqual(JSON.parse(optionsJson), manifest.options)
  ) {
    throw new WaspSpecUserError(
      `Auth provider '${manifest.id}' has options that do not survive JSON serialization. Provider options must be plain serializable data; use setupFn for functions and other live values.`,
    );
  }

  return optionsJson;
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
    userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
  };
}

export function mapSocialAuth(
  socialAuth: WaspSpec.SocialAuthConfig,
  ctx: AppMapperContext,
): AppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = socialAuth;
  return {
    configFn: configFn && ctx.parseRefObject(configFn),
    userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
  };
}

export function mapEmailAuth(
  emailAuth: WaspSpec.EmailAuthConfig,
  ctx: AppMapperContext,
): AppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    emailAuth;
  return {
    userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
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
    getEmailContentFn:
      getEmailContentFn && ctx.parseRefObject(getEmailContentFn),
    clientRoute: ctx.resolveRouteRef(clientRoute),
  };
}

export function mapServer(
  server: WaspSpec.Server,
  ctx: AppMapperContext,
): AppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server;
  return {
    setupFn: setupFn && ctx.parseRefObject(setupFn),
    middlewareConfigFn:
      middlewareConfigFn && ctx.parseRefObject(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && ctx.parseRefObject(envValidationSchema),
  };
}

export function mapClient(
  client: WaspSpec.Client,
  ctx: AppMapperContext,
): AppSpec.Client {
  const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
  return {
    rootComponent: rootComponent && ctx.parseRefObject(rootComponent),
    setupFn: setupFn && ctx.parseRefObject(setupFn),
    baseDir,
    envValidationSchema:
      envValidationSchema && ctx.parseRefObject(envValidationSchema),
  };
}

export function mapDb(db: WaspSpec.Db, ctx: AppMapperContext): AppSpec.Db {
  const { seeds, prismaSetupFn } = db;
  return {
    seeds: seeds?.map(ctx.parseRefObject),
    prismaSetupFn: prismaSetupFn && ctx.parseRefObject(prismaSetupFn),
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
    fn: ctx.parseRefObject(fn),
    autoConnect,
  };
}
