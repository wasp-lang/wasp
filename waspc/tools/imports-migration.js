const util = require('util');

// `same` means that new export is same as old one. Same name, also type, also user defined.
// The only thing that we impliclty assume will change even for `same` is that new exports are always
// considered to be named exports, even if original one was default.
const same = Symbol("SAME")

const importMappings = [
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
  [["@wasp/auth/helpers/GitHub", "SignInButton", "signInUrl"],  // Renaming
   ["wasp/client/auth",          "GitHubSignInButton", "gitHubSignInUrl"]
  ],
  [["@wasp/auth/helpers/Google", "SignInButton", "signInUrl"],  // Renaming
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

  // NOTE: This is a special case because multiple exports are getting replaced with a single export.
  [["@wasp/crud/<MyCrud>", type("GetAllQuery"), type("GetQuery"), type("CreateAction"),
                           type("UpdateAction"), type("DeleteAction")],
   ["wasp/server/crud",    userDef(type("MyCrud")), userDef(type("MyCrud")), userDef(type("MyCrud")),
                           userDef(type("MyCrud")), userDef(type("MyCrud"))]
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
                                "ensureValidUsername"],
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

function resolveImportMapping([oldImport, newImport]) {
  const [oldPath, ...oldNamesUnresolved] = oldImport;
  const oldNames = oldNamesUnresolved.map(resolveName);
  const oldImportResolved = {
    path: oldPath,
    names: oldNames
  }

  if (newImport === null) {
    return { old: oldImportResolved, new: null };
  }

  const [newPath, ...newNamesUnresolved] = newImport;
  const newNames = newNamesUnresolved.map((name, i) => {
    if (name !== same) return resolveName(name);
    if (name === same) {
      const newName = { ...oldNames[i], isSame: true };
      delete newName.isDefault;
      return newName;
    } else {
      return resolveName(name);
    }
  })
  const newImportResolved = {
    path: newPath,
    names: newNames
  }

  return { old: oldImportResolved, new: newImportResolved }
}

function dflt(x) { return "default:" + x; }
function type(x) { return "type:" + x; }
function userDef(x) { return "userDef:" + x; }

function resolveName(s) {
  const tokens = s.split(':')
  const name = tokens.slice(-1)[0]
  const flags = tokens.slice(0, -1)
  return {
    name,
    ...(flags.includes('default') ? { isDefault: true } : null),
    ...(flags.includes('type') ? { isType: true } : null),
    ...(flags.includes('userDef') ? { isUserDef: true } : null),
  }
}

const importMappingsAsObjects = importMappings.map(resolveImportMapping);

function printMappingsAsNiceObjects() {
  for (const mapping of importMappingsAsObjects) {
    console.log(util.inspect(mapping, { depth: null, colors: true }));
  }
}

function printMappingsAsCsv() {
  console.log('oldPath,oldName,isDefault,isType,isUserDef,newPath,newName,isSame');
  for (const m of importMappingsAsObjects) {
    const oldPath = m.old.path;
    const newPath = m.new?.path ?? null;
    for (let i = 0; i < m.old.names.length; i++) {
      const oldName = m.old.names[i];
      const newName = m.new?.names[i] ?? null;
      console.log(`${oldPath},${oldName.name},${!!oldName.isDefault},${!!oldName.isType},${!!oldName.isUserDef},${newPath},${newName?.name ?? null},${!!newName?.isSame}`);
    }
  }
}

printMappingsAsCsv();
