export function mapAppSpec(app, ctx) {
    const { name, wasp, title, deployment, head, auth, server, client, db, emailSender, webSocket, } = app;
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
export function mapDeployment(deployment) {
    return {
        mode: deployment.mode,
    };
}
export function mapAuth(auth, ctx) {
    const { userEntity, methods, onAuthFailedRedirectTo, onAuthSucceededRedirectTo, onBeforeSignup, onAfterSignup, onAfterEmailVerified, onBeforeOAuthRedirect, onBeforeLogin, onAfterLogin, } = auth;
    return {
        userEntity: ctx.resolveEntityRef(userEntity),
        methods: mapAuthMethods(methods, ctx),
        onAuthFailedRedirectTo,
        onAuthSucceededRedirectTo,
        onBeforeSignup: onBeforeSignup && ctx.parseRefObject(onBeforeSignup),
        onAfterSignup: onAfterSignup && ctx.parseRefObject(onAfterSignup),
        onAfterEmailVerified: onAfterEmailVerified && ctx.parseRefObject(onAfterEmailVerified),
        onBeforeOAuthRedirect: onBeforeOAuthRedirect && ctx.parseRefObject(onBeforeOAuthRedirect),
        onBeforeLogin: onBeforeLogin && ctx.parseRefObject(onBeforeLogin),
        onAfterLogin: onAfterLogin && ctx.parseRefObject(onAfterLogin),
    };
}
export function mapAuthMethods(methods, ctx) {
    const { usernameAndPassword, slack, discord, google, gitHub, keycloak, microsoft, email, } = methods;
    return {
        usernameAndPassword: usernameAndPassword && mapUsernameAndPassword(usernameAndPassword, ctx),
        slack: slack && mapSocialAuth(slack, ctx),
        discord: discord && mapSocialAuth(discord, ctx),
        google: google && mapSocialAuth(google, ctx),
        gitHub: gitHub && mapSocialAuth(gitHub, ctx),
        keycloak: keycloak && mapSocialAuth(keycloak, ctx),
        microsoft: microsoft && mapSocialAuth(microsoft, ctx),
        email: email && mapEmailAuth(email, ctx),
    };
}
export function mapUsernameAndPassword(usernameAndPassword, ctx) {
    const { userSignupFields } = usernameAndPassword;
    return {
        userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
    };
}
export function mapSocialAuth(socialAuth, ctx) {
    const { configFn, userSignupFields } = socialAuth;
    return {
        configFn: configFn && ctx.parseRefObject(configFn),
        userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
    };
}
export function mapEmailAuth(emailAuth, ctx) {
    const { userSignupFields, fromField, emailVerification, passwordReset } = emailAuth;
    return {
        userSignupFields: userSignupFields && ctx.parseRefObject(userSignupFields),
        fromField: mapEmailFromField(fromField),
        emailVerification: mapEmailFlow(emailVerification, ctx),
        passwordReset: mapEmailFlow(passwordReset, ctx),
    };
}
export function mapEmailFlow(emailFlow, ctx) {
    const { getEmailContentFn, clientRoute } = emailFlow;
    return {
        getEmailContentFn: getEmailContentFn && ctx.parseRefObject(getEmailContentFn),
        clientRoute: ctx.resolveRouteRef(clientRoute),
    };
}
export function mapServer(server, ctx) {
    const { setupFn, middlewareConfigFn, envValidationSchema, traceDeps } = server;
    return {
        setupFn: setupFn && ctx.parseRefObject(setupFn),
        middlewareConfigFn: middlewareConfigFn && ctx.parseRefObject(middlewareConfigFn),
        envValidationSchema: envValidationSchema && ctx.parseRefObject(envValidationSchema),
        traceDeps,
    };
}
export function mapClient(client, ctx) {
    const { rootComponent, setupFn, baseDir, envValidationSchema } = client;
    return {
        rootComponent: rootComponent && ctx.parseRefObject(rootComponent),
        setupFn: setupFn && ctx.parseRefObject(setupFn),
        baseDir,
        envValidationSchema: envValidationSchema && ctx.parseRefObject(envValidationSchema),
    };
}
export function mapDb(db, ctx) {
    const { seeds, prismaSetupFn } = db;
    return {
        seeds: seeds?.map(ctx.parseRefObject),
        prismaSetupFn: prismaSetupFn && ctx.parseRefObject(prismaSetupFn),
    };
}
export function mapEmailSender(emailSender) {
    const { provider, defaultFrom } = emailSender;
    return {
        provider,
        defaultFrom: defaultFrom && mapEmailFromField(defaultFrom),
    };
}
export function mapEmailFromField(emailFromField) {
    return {
        name: emailFromField.name,
        email: emailFromField.email,
    };
}
export function mapWebSocket(webSocket, ctx) {
    const { fn, autoConnect } = webSocket;
    return {
        fn: ctx.parseRefObject(fn),
        autoConnect,
    };
}
