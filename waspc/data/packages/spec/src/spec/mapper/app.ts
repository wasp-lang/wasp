import { isEqual } from "es-toolkit";
import * as AppSpec from "../../appSpec.js";
import type { AnyObject } from "../../typeUtils.js";
import {
  reservedClientEnvVarNames,
  reservedServerEnvVarNames,
} from "../authReservedEnvVarNames.js";
import {
  isValidProviderId,
  validateIdentityNamespaces,
} from "../publicApi/constructors.js";
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
  const { userEntity, onAuthFailedRedirectTo, providers, hooks } = auth;

  if ("provider" in auth) {
    throw new WaspSpecUserError(
      "app.auth.provider was renamed to app.auth.providers (an array of providers). Wrap your provider in an array: providers: [waspAuth({ ... })] (waspAuth comes from @wasp.sh/auth/spec).",
    );
  }

  if (!Array.isArray(providers) || providers.length === 0) {
    throw new WaspSpecUserError(
      "app.auth.providers must be a non-empty array of providers, each created with an auth adapter package's spec helper (e.g. waspAuth() from @wasp.sh/auth/spec) or customAuthProvider().",
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
    hooks: hooks && {
      onBeforeSignup:
        hooks.onBeforeSignup && ctx.parseRefObject(hooks.onBeforeSignup),
      onAfterSignup:
        hooks.onAfterSignup && ctx.parseRefObject(hooks.onAfterSignup),
      onBeforeLogin:
        hooks.onBeforeLogin && ctx.parseRefObject(hooks.onBeforeLogin),
      onAfterLogin:
        hooks.onAfterLogin && ctx.parseRefObject(hooks.onAfterLogin),
    },
  };
}

function mapAuthProvider(
  provider: WaspSpec.AuthProviderConfig,
  ctx: AppMapperContext,
): AppSpec.AuthProvider {
  if (
    typeof provider !== "object" ||
    provider === null ||
    (provider as { kind?: unknown }).kind !== "external"
  ) {
    throw new WaspSpecUserError(
      "Each entry of app.auth.providers must be created with an auth adapter package's spec helper (e.g. waspAuth() from @wasp.sh/auth/spec) or customAuthProvider().",
    );
  }
  return mapAuthProviderManifest(provider, ctx);
}

// Identities (and sessions) are recorded under the provider id, so a
// duplicate would silently merge two providers' subjects. Checked here so the
// error carries the offending id; the Haskell validator mirrors it.
function assertProviderIdsAreUnique(providers: AppSpec.AuthProvider[]): void {
  const seenIds = new Set<string>();
  for (const provider of providers) {
    const providerId = provider.providerId;
    if (seenIds.has(providerId)) {
      throw new WaspSpecUserError(
        `app.auth.providers contains provider id '${providerId}' more than once. Identities are recorded under this id, so each provider may appear at most once (provider instance ids are not configurable yet).`,
      );
    }
    seenIds.add(providerId);
  }
}

function mapAuthProviderManifest(
  manifest: WaspSpec.AuthProviderManifest,
  ctx: AppMapperContext,
): AppSpec.AuthProviderSpec {
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

  // The rules below repeat defineAuthProviderManifest's checks on purpose:
  // the authenticity marker is an ordinary property, so a manifest built as an
  // object literal can carry it without ever passing those checks. The mapper
  // is the layer no manifest can skip; the Haskell validator mirrors these
  // rules once more for the non-TS entry points.
  if (!isValidProviderId(manifest.id)) {
    throw new WaspSpecUserError(
      `Auth provider id '${manifest.id}' must be non-empty and contain no ':' -- the ':' separates a provider id from its identity namespaces ('wasp:email').`,
    );
  }
  if (
    manifest.capabilities.includes("cookie-transport") &&
    !manifest.capabilities.includes("session-revocation")
  ) {
    throw new WaspSpecUserError(
      `Auth provider '${manifest.id}' declares the 'cookie-transport' capability without 'session-revocation'. A provider whose credential lives in a cookie must be able to revoke sessions server-side, or logout would only appear to work.`,
    );
  }
  for (const [side, envVars, reservedNames] of [
    ["server", manifest.env.server, reservedServerEnvVarNames],
    ["client", manifest.env.client, reservedClientEnvVarNames],
  ] as const) {
    for (const envVar of envVars) {
      if (reservedNames.includes(envVar.name)) {
        throw new WaspSpecUserError(
          `Auth provider '${manifest.id}' declares the ${side} env var '${envVar.name}', which Wasp owns. Framework env var names cannot be declared by providers; pick a provider-specific name.`,
        );
      }
    }
  }
  const uses = manifest.uses ?? [];
  for (const grant of uses) {
    if (
      !["wasp-sessions", "email-send", "identity-namespaces"].includes(grant)
    ) {
      throw new WaspSpecUserError(
        `Auth provider '${manifest.id}' requests the unknown runtime grant '${String(
          grant,
        )}'. Known grants: wasp-sessions, email-send, identity-namespaces.`,
      );
    }
  }
  const identityNamespaces = manifest.identityNamespaces ?? [manifest.id];
  validateIdentityNamespaces(manifest.id, identityNamespaces, uses);

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
    uses,
    identityNamespaces,
    userSignupFields:
      manifest.userSignupFields &&
      ctx.parseRefObject(manifest.userSignupFields),
    setupFn: manifest.setupFn && ctx.parseRefObject(manifest.setupFn),
    extensions: Object.fromEntries(
      Object.entries(manifest.extensions ?? {}).map(([name, ref]) => [
        name,
        ctx.parseRefObject(ref),
      ]),
    ),
    optionsJson: mapProviderOptions(manifest),
  };
}

function mapEnvVarRequirement(
  envVar: WaspSpec.EnvVarRequirement,
): AppSpec.ExternalProviderEnvVar {
  return {
    name: envVar.name,
    optional: envVar.optional,
    doc: envVar.doc,
    devDefault: envVar.devDefault,
  };
}

function mapProviderOptions(
  manifest: WaspSpec.AuthProviderManifest,
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
