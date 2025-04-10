import { App, ExtImport } from 'wasp-config'

const app = new App('Thoughts', {
  title: 'Thoughts',
  wasp: { version: '^0.16.0' }
});

app.db({
  seeds: [{ import: 'devSeedBasic', from: '@src/server/seeds.js' }]
});

app.auth({
  userEntity: 'User',
  methods: { usernameAndPassword: {} },
  onAuthFailedRedirectTo: '/login'
});

appPageWithRoute('Main',     '/',         '@src/client/MainPage.jsx',     { auth: true });
appPageWithRoute('Thoughts', '/thoughts', '@src/client/ThoughtsPage.jsx', { auth: true });
appPageWithRoute('Login',    '/login',    '@src/client/LoginPage.jsx');
appPageWithRoute('Signup',   '/signup',   '@src/client/SignupPage.jsx');

appAction('createThought', ['Thought', 'Tag'], '@src/server/actions.js');
appQuery( 'getTags',       ['Tag'],            '@src/server/queries.js');
appQuery( 'getThoughts',   ['Thought'],        '@src/server/queries.js');

app.api('time', {
  fn: {
    from: '@src/server/apis.js',
    import: 'getCurrentTime',
  },
  httpRoute: {
    method: 'GET',
    route: '/time',
  },
});

app.apiNamespace('time', {
  path: '/time',
  middlewareConfigFn: {
    from: '@src/server/apis.js',
    import: 'apiMiddleware',
  },
});

function appPageWithRoute(pageName: string, path: string, from: ExtImport['from'], config?: { auth: boolean }) {
  const page = app.page(pageName, {
    component: { importDefault: pageName, from },
    ...(config?.auth && { authRequired: config?.auth })
  });
  app.route(pageName + 'Route', { path, to: page });
}

function appQuery(jsName: string, entities: string[], from: ExtImport['from']) {
  app.query(jsName, {
    fn: { import: jsName, from },
    entities
  });
}

function appAction(jsName: string, entities: string[], from: ExtImport['from']) {
  app.action(jsName, {
    fn: { import: jsName, from },
    entities
  });
}

export default app;
