import { isEqual } from "es-toolkit";
import * as AppSpec from "../../appSpec.js";
import type { AnyFunction } from "../../typeUtils.js";
import {
  reservedClientEnvVarNames,
  reservedServerEnvVarNames,
} from "../authReservedEnvVarNames.js";
import {
  isValidSchemeName,
  validateCredentialsConfig,
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
    deployment,
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
      deployment: deployment && mapDeployment(deployment),
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

export function mapDeployment(
  deployment: WaspSpec.Deployment,
): AppSpec.Deployment {
  return {
    mode: deployment.mode,
  };
}

export function mapAuth(
  auth: WaspSpec.Auth,
  ctx: AppMapperContext,
): AppSpec.Auth {
  const { userEntity, onAuthFailedRedirectTo, schemes, hooks } = auth;

  if ("providers" in auth || "provider" in auth) {
    throw new WaspSpecUserError(
      "app.auth.providers was replaced by app.auth.schemes, an object keyed by scheme name: schemes: { wasp: waspAuth({ ... }) } (waspAuth comes from @wasp.sh/auth/spec).",
    );
  }
  if ("methods" in auth) {
    throw new WaspSpecUserError(
      "app.auth.methods moved into Wasp's auth package: schemes: { wasp: waspAuth({ methods: { ... } }) } (waspAuth comes from @wasp.sh/auth/spec).",
    );
  }

  if (
    typeof schemes !== "object" ||
    schemes === null ||
    Array.isArray(schemes) ||
    Object.keys(schemes).length === 0
  ) {
    throw new WaspSpecUserError(
      "app.auth.schemes must be a non-empty object keyed by scheme name, each value created with an auth handler package's spec helper (e.g. waspAuth() from @wasp.sh/auth/spec), waspBearer()/waspCookie(), or customAuthHandler().",
    );
  }

  const mappedSchemes = Object.entries(schemes).map(([name, manifest]) =>
    mapAuthScheme(name, manifest, ctx),
  );
  const defaultScheme = resolveDefaultScheme(auth, mappedSchemes);
  validateCredentialTargets(mappedSchemes);

  return {
    userEntity: ctx.resolveEntityRef(userEntity),
    onAuthFailedRedirectTo,
    schemes: mappedSchemes,
    defaultScheme,
    mergeUsers: auth.mergeUsers && ctx.parseRefObject(auth.mergeUsers),
    hooks: hooks && {
      onBeforeSignup:
        hooks.onBeforeSignup && ctx.parseRefObject(hooks.onBeforeSignup),
      onAfterSignup:
        hooks.onAfterSignup && ctx.parseRefObject(hooks.onAfterSignup),
      onBeforeLogin:
        hooks.onBeforeLogin && ctx.parseRefObject(hooks.onBeforeLogin),
      onAfterLogin:
        hooks.onAfterLogin && ctx.parseRefObject(hooks.onAfterLogin),
      onBeforeLink:
        hooks.onBeforeLink && ctx.parseRefObject(hooks.onBeforeLink),
      onAfterLink: hooks.onAfterLink && ctx.parseRefObject(hooks.onAfterLink),
    },
  };
}

function resolveDefaultScheme(
  auth: WaspSpec.Auth,
  schemes: AppSpec.AuthScheme[],
): string {
  const names = schemes.map((scheme) => scheme.name);
  if (auth.default !== undefined) {
    if (!names.includes(auth.default)) {
      throw new WaspSpecUserError(
        `app.auth.default names the scheme '${auth.default}', which app.auth.schemes does not declare. Declared: ${names.join(", ")}.`,
      );
    }
    return auth.default;
  }
  const [onlyScheme] = names;
  if (names.length === 1 && onlyScheme !== undefined) {
    return onlyScheme;
  }
  throw new WaspSpecUserError(
    `app.auth declares several schemes (${names.join(", ")}), so app.auth.default must name the one that authenticates plain authRequired: true assets.`,
  );
}

// A credentials scheme must exist, must be able to issue credentials, and the
// chain must end: a scheme cannot sign into itself, nor into a scheme that
// (transitively) signs into it.
function validateCredentialTargets(schemes: AppSpec.AuthScheme[]): void {
  const byName = new Map(schemes.map((scheme) => [scheme.name, scheme]));
  for (const scheme of schemes) {
    const credentials = scheme.credentials;
    if (credentials === undefined || !("scheme" in credentials)) {
      continue;
    }
    const target = byName.get(credentials.scheme);
    if (target === undefined) {
      throw new WaspSpecUserError(
        `Auth scheme '${scheme.name}' signs into '${credentials.scheme}', which app.auth.schemes does not declare.`,
      );
    }
    if (!target.capabilities.includes("sign-in")) {
      throw new WaspSpecUserError(
        `Auth scheme '${scheme.name}' signs into '${credentials.scheme}', but that scheme's handler ('${target.handler}') does not declare the 'sign-in' capability.`,
      );
    }
    // Walk the chain from the target; hitting this scheme again is a cycle.
    const seen = new Set<string>([scheme.name]);
    let current: AppSpec.AuthScheme | undefined = target;
    while (current !== undefined) {
      if (seen.has(current.name)) {
        throw new WaspSpecUserError(
          `Auth scheme '${scheme.name}' signs into '${credentials.scheme}', which leads back to itself. A credentials chain must end in a scheme that issues its own credentials.`,
        );
      }
      seen.add(current.name);
      const next: AppSpec.AuthSchemeCredentials | undefined =
        current.credentials;
      current =
        next !== undefined && "scheme" in next
          ? byName.get(next.scheme)
          : undefined;
    }
  }
}

function mapAuthScheme(
  name: string,
  manifest: WaspSpec.AuthSchemeManifest,
  ctx: AppMapperContext,
): AppSpec.AuthScheme {
  if (!isValidSchemeName(name)) {
    throw new WaspSpecUserError(
      `Auth scheme name '${name}' must be non-empty and contain neither ':' (the identity namespace separator) nor '/' (it names the scheme's routes).`,
    );
  }
  if (
    typeof manifest !== "object" ||
    manifest === null ||
    (manifest as { kind?: unknown }).kind !== "scheme"
  ) {
    throw new WaspSpecUserError(
      `Auth scheme '${name}' must be created with an auth handler package's spec helper (e.g. waspAuth() from @wasp.sh/auth/spec), waspBearer()/waspCookie(), or customAuthHandler().`,
    );
  }
  if (manifest.__waspAuthSchemeManifest !== true) {
    throw new WaspSpecUserError(
      `Auth scheme '${name}' received a hand-crafted manifest. Manifests must be created through a handler package's spec helper or customAuthHandler(), so they go through Wasp's validation.`,
    );
  }
  if (manifest.contractVersion !== 3) {
    throw new WaspSpecUserError(
      `Auth scheme '${name}' (handler '${manifest.handler}') was built against auth contract version ${String(
        manifest.contractVersion,
      )}, but this version of Wasp only supports version 2. Update Wasp, or use a handler version matching your Wasp version.`,
    );
  }

  // The rules below repeat defineAuthSchemeManifest's checks on purpose: the
  // authenticity marker is an ordinary property, so a manifest built as an
  // object literal can carry it without ever passing those checks. The mapper
  // is the layer no manifest can skip; the Haskell validator mirrors these
  // rules once more for the non-TS entry points.
  for (const [side, envVars, reservedNames] of [
    ["server", manifest.env.server, reservedServerEnvVarNames],
    ["client", manifest.env.client, reservedClientEnvVarNames],
  ] as const) {
    for (const envVar of envVars) {
      if (reservedNames.includes(envVar.name)) {
        throw new WaspSpecUserError(
          `Auth scheme '${name}' declares the ${side} env var '${envVar.name}', which Wasp owns. Framework env var names cannot be declared by handlers; pick a handler-specific name.`,
        );
      }
    }
  }
  const uses = manifest.uses ?? [];
  for (const grant of uses) {
    if (!["email-send", "identity-namespaces"].includes(grant)) {
      throw new WaspSpecUserError(
        `Auth scheme '${name}' requests the unknown runtime grant '${String(
          grant,
        )}'. Known grants: email-send, identity-namespaces.`,
      );
    }
  }
  const namespaceSuffixes = manifest.identityNamespaces ?? [];
  validateIdentityNamespaces(manifest.handler, namespaceSuffixes, uses);
  if (manifest.credentials !== undefined) {
    validateCredentialsConfig(manifest.handler, manifest.credentials);
  }

  // Reserved for a future in which handler packages contribute Prisma models.
  // Erroring (rather than ignoring) means a handler relying on them can never
  // appear to work while its models silently don't exist.
  for (const reservedField of ["prismaModels", "manageSchema"]) {
    if (reservedField in manifest) {
      throw new WaspSpecUserError(
        `Auth scheme '${name}' sets '${reservedField}', which this version of Wasp does not support yet.`,
      );
    }
  }

  // Both halves take the same two forms: a package entry, or a reference to
  // a factory in the app's own code.
  const mapEntry = (
    entry: { package: string } | WaspSpec.Reference<AnyFunction>,
  ): { package: string } | { module: AppSpec.ExtImport } =>
    "package" in entry
      ? { package: entry.package }
      : { module: ctx.parseRefObject(entry) };

  return {
    name,
    handler: manifest.handler,
    server: mapEntry(manifest.server),
    client: manifest.client && mapEntry(manifest.client),
    routes: manifest.routes && { rawBody: manifest.routes.rawBody },
    capabilities: manifest.capabilities,
    envVars: {
      server: manifest.env.server.map(mapEnvVarRequirement),
      client: manifest.env.client.map(mapEnvVarRequirement),
    },
    uses,
    identityNamespaces: [
      name,
      ...namespaceSuffixes.map((suffix) => `${name}:${suffix}`),
    ],
    credentials:
      manifest.credentials && mapCredentials(manifest.credentials, ctx),
    userSignupFields:
      manifest.userSignupFields &&
      ctx.parseRefObject(manifest.userSignupFields),
    setupFn: manifest.setupFn && ctx.parseRefObject(manifest.setupFn),
    extensions: Object.fromEntries(
      Object.entries(manifest.extensions ?? {}).map(([extName, ref]) => [
        extName,
        ctx.parseRefObject(ref),
      ]),
    ),
    optionsJson: mapSchemeOptions(name, manifest),
  };
}

function mapCredentials(
  credentials: WaspSpec.CredentialsConfig,
  ctx: AppMapperContext,
): AppSpec.AuthSchemeCredentials {
  if ("scheme" in credentials) {
    return { scheme: credentials.scheme };
  }
  const store = credentials.store ?? "prisma";
  return {
    transport: credentials.transport ?? "bearer",
    store:
      typeof store === "string" ? store : { module: ctx.parseRefObject(store) },
    ttl: credentials.ttl ?? "30d",
  };
}

function mapEnvVarRequirement(
  envVar: WaspSpec.EnvVarRequirement,
): AppSpec.AuthSchemeEnvVar {
  return {
    name: envVar.name,
    optional: envVar.optional,
    doc: envVar.doc,
    devDefault: envVar.devDefault,
  };
}

function mapSchemeOptions(
  name: string,
  manifest: WaspSpec.AuthSchemeManifest,
): string | undefined {
  if (manifest.options === undefined) {
    return undefined;
  }

  // Options travel to the generated code as JSON, so anything that doesn't
  // survive the round-trip (functions, class instances, undefined-holed
  // arrays) would arrive silently mangled. Rejecting here turns that into an
  // error at compile time, with `setupFn` and `extensions` as the documented
  // escape hatches for non-serializable configuration.
  const optionsJson = JSON.stringify(manifest.options);
  if (
    optionsJson === undefined ||
    !isEqual(JSON.parse(optionsJson), manifest.options)
  ) {
    throw new WaspSpecUserError(
      `Auth scheme '${name}' has options that do not survive JSON serialization. Handler options must be plain serializable data; use setupFn or extensions for functions and other live values.`,
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
