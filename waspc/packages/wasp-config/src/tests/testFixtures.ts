// File explanation:
// https://stackoverflow.com/questions/12071344/what-are-fixtures-in-programming

export const APP = {
  NAME: 'todoApp',
  TITLE: 'ToDO App',
  VERSION: '^0.16.0',
  HEAD_FAVICON: '<link rel="icon" href="/favicon.ico" />',
} as const

export const CRUD = {
  NAME: 'TaskCrud',
  ENTITY: 'Task',
  OPERATIONS: {
    GET: {
      IS_PUBLIC: true,
      OVERRIDE_FN: {
        IMPORT: 'getTask',
        FROM: '@src/cruds/task',
      },
    },
    GET_ALL: {
      IS_PUBLIC: true,
    },
    CREATE: {
      IS_PUBLIC: false,
    },
    UPDATE: {
      IS_PUBLIC: false,
    },
    DELETE: {
      IS_PUBLIC: false,
      OVERRIDE_FN: {
        IMPORT: 'deleteTask',
        FROM: '@src/cruds/task',
      },
    },
  },
} as const

export const PAGE = {
  LOGIN: {
    NAME: 'LoginPage',
    COMPONENT: {
      FROM: '@src/pages/auth/Login',
      IMPORT: 'Login',
    },
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationPage',
    COMPONENT: {
      FROM: '@src/pages/auth/EmailVerification',
      IMPORT: 'EmailVerification',
    },
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetPage',
    COMPONENT: {
      FROM: '@src/pages/auth/PasswordReset',
      IMPORT: 'PasswordReset',
    },
  },
} as const

export const ROUTE = {
  LOGIN: {
    NAME: 'LoginRoute',
    PATH: '/login',
    TO: PAGE.LOGIN.NAME,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationRoute',
    PATH: '/email-verification',
    TO: PAGE.EMAIL_VERIFICATION.NAME,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetRoute',
    PATH: '/password-reset',
    TO: PAGE.PASSWORD_RESET.NAME,
  },
} as const

export const API_NAMESPACE = {
  NAME: 'bar',
  MIDDLEWARE_CONFIG_FN: {
    IMPORT: 'barMiddlewareConfigFn',
    FROM: '@src/apis',
  },
  PATH: '/bar',
} as const

export const API = {
  NAME: 'barBaz',
  FN: {
    IMPORT: 'barBaz',
    FROM: '@src/apis',
  },
  AUTH: true,
  HTTP_ROUTE: {
    METHOD: 'GET',
    ROUTE: '/bar/baz',
  },
  ENTITY: 'Task',
} as const

export const ACTION = {
  NAME: 'createTask',
  FROM: '@src/actions',
  ENTITY: 'Task',
} as const

export const QUERY = {
  NAME: 'getTasks',
  FROM: '@src/queries',
  ENTITY: 'Task',
} as const

export const JOB = {
  NAME: 'mySpecialJob',
  EXECUTOR: 'PgBoss',
  ENTITY: 'Task',
  PERFORM: {
    FN: {
      IMPORT: 'perform',
      FROM: '@src/jobs/bar',
    },
  },
} as const

export const AUTH = {
  NAME: 'Auth',
  ENTITY: 'User',
  METHODS: {
    DISCORD: {
      CONFIG_FN: {
        IMPORT: 'config',
        FROM: '@src/auth/discord',
      },
      USER_SIGNUP_FIELDS: {
        IMPORT: 'userSignupFields',
        FROM: '@src/auth/discord',
      },
    },

    GOOGLE: {
      CONFIG_FN: {
        IMPORT: 'config',
        FROM: '@src/auth/google',
      },
      USER_SIGNUP_FIELDS: {
        IMPORT: 'userSignupFields',
        FROM: '@src/auth/google',
      },
    },

    GITHUB: {
      CONFIG_FN: {
        IMPORT: 'config',
        FROM: '@src/auth/github',
      },
      USER_SIGNUP_FIELDS: {
        IMPORT: 'userSignupFields',
        FROM: '@src/auth/github',
      },
    },

    KEYCLOAK: {
      CONFIG_FN: {
        IMPORT: 'config',
        FROM: '@src/auth/keycloak',
      },
      USER_SIGNUP_FIELDS: {
        IMPORT: 'userSignupFields',
        FROM: '@src/auth/keycloak',
      },
    },

    USERNAME_AND_PASSWORD: {
      USER_SIGNUP_FIELDS: {
        IMPORT: 'userSignupFields',
        FROM: '@src/auth/usernameAndPassword',
      },
    },

    EMAIL: {
      USER_SIGNUP_FIELDS: {
        IMPORT: 'userSignupFields',
        FROM: '@src/auth/email',
      },
      FROM_FIELD: {
        NAME: 'ToDo App',
        EMAIL: 'test@domain.tld',
      },
      EMAIL_VERIFICATION: {
        GET_EMAIL_CONTENT_FN: {
          IMPORT: 'getVerificationEmailContent',
          FROM: '@src/auth/email',
        },
        CLIENT_ROUTE: ROUTE.EMAIL_VERIFICATION.NAME,
      },
      PASSWORD_RESET: {
        GET_EMAIL_CONTENT_FN: {
          IMPORT: 'getPasswordResetEmailContent',
          FROM: '@src/auth/email',
        },
        CLIENT_ROUTE: ROUTE.PASSWORD_RESET.NAME,
      },
    },
  },
  ON_AUTH_FAILED_REDIRECT_TO: '/login',
  ON_AUTH_SUCCEEDED_REDIRECT_TO: '/profile',
  ON_BEFORE_SIGNUP: {
    IMPORT: 'onBeforeSignup',
    FROM: '@src/auth/hooks.js',
  },
  ON_AFTER_SIGNUP: {
    IMPORT: 'onAfterSignup',
    FROM: '@src/auth/hooks.js',
  },
  ON_BEFORE_OAUTH_REDIRECT: {
    IMPORT: 'onBeforeOAuthRedirect',
    FROM: '@src/auth/hooks.js',
  },
  ON_BEFORE_LOGIN: {
    IMPORT: 'onBeforeLogin',
    FROM: '@src/auth/hooks.js',
  },
  ON_AFTER_LOGIN: {
    IMPORT: 'onAfterLogin',
    FROM: '@src/auth/hooks.js',
  },
} as const

export const CLIENT = {
  ROOT_COMPONENT: {
    FROM: '@src/App',
    IMPORT: 'App',
  },
  SETUP_FN: {
    FROM: '@src/clientSetup',
    IMPORT: 'setup',
  },
} as const

export const SERVER = {
  SETUP_FROM: '@src/serverSetup',
  SETUP_IMPORT_DEFAULT: 'setup',
  MIDDLEWARE_IMPORT: 'serverMiddlewareFn',
} as const

export const WEBSOCKET = {
  FROM: '@src/webSocket',
  FN_IMPORT: 'webSocketFn',
  AUTO_CONNECT: true,
} as const

export const DB = {
  SEEDS: {
    FROM: '@src/dbSeeds',
    IMPORT: 'devSeedSimple',
  },
} as const

export const EMAIL = {
  SMTP: {
    PROVIDER: 'SMTP',
    ADDRESS: 'test@test.com',
  },
} as const
