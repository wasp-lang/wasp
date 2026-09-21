import * as AppSpec from "../../appSpec.js";
import {
  describeAuthHandler,
  isValidSchemeName,
  supportedAuthContractVersion,
  validateCredentialsConfig,
  validateIdentityNamespaces,
  validateSideEnvVars,
} from "../publicApi/constructors.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { WaspSpecUserError } from "../waspSpecUserError.js";
import { AppMapperContext } from "./context.js";
import { splitHandlerSpec } from "./handlerSpec.js";

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
  const handler = describeAuthHandler(manifest);
  if (manifest.contractVersion !== supportedAuthContractVersion) {
    throw new WaspSpecUserError(
      `Auth scheme '${name}' (handler '${handler}') was built against auth contract version ${String(
        manifest.contractVersion,
      )}, but this version of Wasp only supports version ${supportedAuthContractVersion}. Update Wasp, or use a handler version matching your Wasp version.`,
    );
  }

  validateSideEnvVars(handler, manifest);
  const uses = manifest.uses ?? [];
  for (const grant of uses) {
    if (!["email-send"].includes(grant)) {
      throw new WaspSpecUserError(
        `Auth scheme '${name}' requests the unknown runtime grant '${String(
          grant,
        )}'. Known grants: email-send.`,
      );
    }
  }
  const namespaceSuffixes = manifest.identityNamespaces ?? [];
  validateIdentityNamespaces(handler, namespaceSuffixes);
  if (manifest.credentials !== undefined) {
    validateCredentialsConfig(handler, manifest.credentials);
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
  // an adapter in the app's own code.
  const mapSide = (
    side: "server" | "client",
    sideManifest: WaspSpec.AuthSchemeServerSide | WaspSpec.AuthSchemeClientSide,
  ): AppSpec.AuthSchemeSide => {
    const entry = sideManifest.authAdapter;
    const handlerSpec = splitHandlerSpec(name, side, sideManifest.spec);
    return {
      // Both forms are the same thing in different places: a package entry
      // (with the conventional export name as the default), or an adapter in
      // the app's own code.
      authAdapter:
        "package" in entry
          ? {
              package: entry.package,
              export: entry.export ?? defaultAdapterExportNames[side],
            }
          : { module: ctx.parseRefObject(entry) },
      envVars: (sideManifest.env ?? []).map(mapEnvVarRequirement),
      specJson: handlerSpec.dataJson,
      specReferences: Object.fromEntries(
        Object.entries(handlerSpec.references).map(([path, ref]) => [
          path,
          ctx.parseRefObject(ref),
        ]),
      ),
    };
  };

  return {
    name,
    handler,
    server: mapSide("server", manifest.server),
    client: manifest.client && mapSide("client", manifest.client),
    routes: manifest.server.routes && {
      rawBody: manifest.server.routes.rawBody,
    },
    capabilities: manifest.capabilities,
    uses,
    // No bare namespace: a manifest that names none gets `default`.
    identityNamespaces: (namespaceSuffixes.length > 0
      ? namespaceSuffixes
      : ["default"]
    ).map((suffix) => `${name}:${suffix}`),
    credentials:
      manifest.credentials && mapCredentials(manifest.credentials, ctx),
    userFieldsFromClaims:
      manifest.userFieldsFromClaims &&
      ctx.parseRefObject(manifest.userFieldsFromClaims),
  };
}

// What a handler package's entries export when the manifest names no export.
const defaultAdapterExportNames = {
  server: "createServerAuthHandler",
  client: "createClientAuthHandler",
} as const;

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
