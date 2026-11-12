import * as AppSpec from "../../appSpec.js";
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
    declValue: {
      name,
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
    userEntity: ctx.resolveEntityRef(userEntity),
    methods: mapAuthMethods(methods, ctx),
    onAuthFailedRedirectTo,
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
  const { userSignupFields, passwordValidationSchema } = usernameAndPassword;
  return {
    userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
    passwordValidationSchema:
      passwordValidationSchema && ctx.parseRefObject(passwordValidationSchema),
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
  const {
    userSignupFields,
    passwordValidationSchema,
    fromField,
    emailVerification,
    passwordReset,
  } = emailAuth;
  return {
    userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
    passwordValidationSchema:
      passwordValidationSchema && ctx.parseRefObject(passwordValidationSchema),
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
  const { setupFn, middlewareConfigFn, envValidationSchema, traceDeps } =
    server;
  return {
    setupFn: setupFn && ctx.parseRefObject(setupFn),
    middlewareConfigFn:
      middlewareConfigFn && ctx.parseRefObject(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && ctx.parseRefObject(envValidationSchema),
    traceDeps,
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
  const mailbox =
    emailSender.provider === "Dummy" ? emailSender.mailbox : undefined;
  return {
    provider,
    defaultFrom: defaultFrom && mapEmailFromField(defaultFrom),
    mailbox: mailbox && mapDevMailboxConfig(mailbox),
  };
}

export function mapDevMailboxConfig(
  devMailboxConfig: WaspSpec.DevMailboxConfig,
): AppSpec.DevMailboxConfig {
  const { maxEmails } = devMailboxConfig;
  return {
    maxEmails: maxEmails === undefined ? undefined : mapMaxEmails(maxEmails),
  };
}

/** JSON has no `Infinity`. */
function mapMaxEmails(maxEmails: number): number | "unlimited" {
  if (maxEmails === Infinity) {
    return "unlimited";
  }
  if (!Number.isInteger(maxEmails) || maxEmails < 0) {
    throw new WaspSpecUserError(
      `app.emailSender.mailbox.maxEmails must be a non-negative integer or Infinity, got ${maxEmails}.`,
    );
  }
  return maxEmails;
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
