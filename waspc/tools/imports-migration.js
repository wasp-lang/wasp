
function dflt(x) { return x; }
function type(x) { return x; }
function userDef(x) { return x; }

// `same` means that new export is same as old one. Same name, also type, also user defined.
// The only thing that we impliclty assume will change even for `same` is that new exports are always
// considered to be named exports, even if original one was default.
const same = Symbol("SAME")

const refactoringRules = [
  [["@wasp/config.js", dflt("config")],
   ["wasp/server",     same]
  ],
  [["@wasp/dbClient.js", dflt("prismaClient")],    // We renamed prismaClient to prisma.
   ["wasp/server",       "prisma"]
  ],

  [["@wasp/utils", "isPrismaError", "prismaErrorToHttpError"],   // These don't exist anymore.
   null
  ],

  [["@wasp/actions",          "useAction", type("OptimisticUpdateDefinition")],
   ["wasp/client/operations", same, same]
  ],
  [["@wasp/actions/<myAction>", userDef(dflt("myAction"))],
   ["wasp/client/operations",   same]
  ],
  [["@wasp/actions/types",    userDef(type("MyAction"))],
   ["wasp/server/operations", same]
  ],
  [["@wasp/queryClient",      "configureQueryClient"],
   ["wasp/client/operations", same]
  ],
  [["@wasp/queries",          "useQuery"],
   ["wasp/client/operations", same]
  ],
  [["@wasp/queries/<myQuery>", userDef(dflt("myQuery"))],
   ["wasp/client/operations",  same]
  ],
  [["@wasp/queries/types",    userDef(type("MyQuery"))],
   ["wasp/server/operations", same]
  ],

  [["@wasp/api",       dflt("api")],
   ["wasp/client/api", same]
  ],
  [["@wasp/apis/types", userDef(type("MyApi"))],
   ["wasp/server/api",  same]
  ],

  [["@wasp/auth/login", dflt("login")],
   ["wasp/client/auth", same]
  ],
  [["@wasp/auth/logout", "logout"],
   ["wasp/client/auth",  same]
  ],
  [["@wasp/auth/signup", dflt("signup")],
   ["wasp/client/auth",  same]
  ],
  [["@wasp/auth/useAuth", dflt("useAuth")],
   ["wasp/client/auth",   same]
  ],
  [["@wasp/auth/email/actions", "requestPasswordReset", "resetPassword", "verifyEmail", "login", "signup"],
   ["wasp/client/auth",         same, same, same, same, same]
  ],
  [["@wasp/auth/providers/email/utils.js", "createEmailVerificationLink", "sendEmailVerificationEmail"],
   ["wasp/server/auth",                    same, same]
  ],
  [["@wasp/types",      type("GetVerificationEmailContentFn"), type("GetPasswordResetEmailContentFn")],
   ["wasp/server/auth", same, same]
  ],
  [["@wasp/auth/forms/ForgotPassword", "ForgotPasswordForm"],
   ["wasp/client/auth",                same]
  ],
  [["@wasp/auth/forms/Login", "LoginForm"],
   ["wasp/client/auth",       same]
  ],
  [["@wasp/auth/forms/ResetPassword", "ResetPasswordForm"],
   ["wasp/client/auth",               same]
  ],
  [["@wasp/auth/forms/Signup", "SignupForm"],
   ["wasp/client/auth",        same]
  ],
  [["@wasp/auth/forms/VerifyEmail", "VerifyEmailForm"],
   ["wasp/client/auth",             same]
  ],
  [["@wasp/auth/forms/types", type("CustomizationOptions")],
   ["wasp/client/auth",       same]
  ],
  [["@wasp/auth/helpers/GitHub", "SignInButton", "signInUrl"],
   ["wasp/client/auth",          "GitHubSignInButton", "gitHubSignInUrl"]
  ],
  [["@wasp/auth/helpers/Google", "SignInButton", "signInUrl"],
   ["wasp/client/auth",          "GoogleSignInButton", "googleSignInUrl"]
  ],

  [["@wasp/core/AuthError", dflt("AuthError")],
   ["wasp/server",          same]
  ],
  [["@wasp/core/HttpError", dflt("HttpError")],
   ["wasp/server",          same]
  ],
  [["@wasp/dbSeed/types.js", type("DbSeedFn")],
   ["wasp/server",           same]
  ],
  [["@wasp/middleware", type("MiddlewareConfigFn")],
   ["wasp/server",      same]
  ],
  [["@wasp/types", type("ServerSetupFn")],
   ["wasp/server", same]
  ],

  [["@wasp/email", "emailSender"],
   ["wasp/email",  same]
  ],

  // NOTE: I think this one they will have to migrate manually, because multiple exports
  //   are getting replaced with a single export. Not sure if we can easily define this programmatically.
  [["@wasp/crud/<MyCrud>", type("GetAllQuery"), type("GetQuery"), type("CreateAction"),
                           type("UpdateAction"), type("DeleteAction")],
   ["wasp/server/crud",    userDef(type("MyCrud"))]
  ],
  [["@wasp/crud/<MyCrud>", "Crud"],
   ["wasp/client/crud",    userDef("MyCrud")]
  ],

  [["@wasp/entities", userDef(type("MyEntity"))],
   ["wasp/entities",  same]
  ],

  [["@wasp/jobs/<MyJob>", userDef("myJob")],
   ["wasp/server/jobs",   same]
  ],

  [["@wasp/router",       "Link", "routes"],
   ["wasp/client/router", same, same]
  ],

  [["@wasp/test",       "mockServer", "renderInContext"],
   ["wasp/client/test", same, same]
  ],

  // NOTE: A bit unique situation, because we replace them with external import from `express`,
  // not a new wasp import.
  [["@wasp/types", type("Application"), type("Express")],
   ["express",     same, same]
  ],

  [["@wasp/webSocket",       type("WebSocketDefinition"), type("WaspSocketData")],
   ["wasp/server/webSocket", same, same]
  ],

  // -------- NEW: came with new Auth -------- //
  [["@wasp/auth",       "defineUserSignupFields"],
   ["wasp/server/auth", same]
  ],
  [["@wasp/auth/user", "getEmail", "getUsername", "getFirstProviderUserId", "findUserIdentity"],
   ["wasp/auth",       same, same, same, same]
  ],
  [["@wasp/auth/types", type("User")],  // User got renamed to AuthUser.
   ["wasp/auth",        type("AuthUser")]
  ],
  [["@wasp/auth/validation.js", "ensurePasswordIsPresent", "ensureValidPassword", "ensureValidEmail",
                                "ensureValidUsername"]
   ["wasp/server/auth",         same, same, same, same]
  ],
  [["@wasp/auth/utils.js", "createProviderId", "sanitizeAndSerializeProviderData", "updateAuthIdentityProviderData",
                           "deserializeAndSanitizeProviderData", "findAuthIdentity", "createUser"],
   ["wasp/server/auth",    same, same, same, same, same, same]
  ],
  [["@wasp/auth/forms/internal/Form", "FormError", "FormInput", "FormItemGroup", "FormLabel"],
   ["wasp/client/auth",               same, same, same, same]
  ],
  // ----------------------------------------- //
]

// There are also some completely new imports!
// TODO: List them here?
