
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

  [["@wasp/queryClient",      "configureQueryClient"],
   ["wasp/client/operations", same]
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

  [["@wasp/email", "emailSender"],
   ["wasp/email",  same]
  ],

  // TODO: Stopped in https://www.notion.so/wasp-lang/Imports-2c9595550213449897cee07064b6ff4b at
  //   import { SignInButton as GitHubSignInButton, signInUrl as gitHubSignInUrl } from @wasp/auth/helpers/GitHub

  // Template, TODO: remove once done:
  [["@wasp/ ", dflt("")],
   ["wasp/ ", ""]
  ],
]

// There are also some completely new imports!
// TODO: List them here?
