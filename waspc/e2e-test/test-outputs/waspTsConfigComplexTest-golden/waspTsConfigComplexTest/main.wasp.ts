import { App } from 'wasp-config'

const app = new App('waspComplexTest', {
  title: 'waspComplexTest',
  wasp: { version: '^0.15.2' },
});

app.auth({
  userEntity: 'User',
  methods: {
    google: {}
  },
  onAuthFailedRedirectTo: '/login',
  onBeforeSignup: { import: 'onBeforeSignup', from: '@src/auth/hooks.js' },
  onAfterSignup: { import: 'onAfterSignup', from: '@src/auth/hooks.js' },
  onBeforeOAuthRedirect: { import: 'onBeforeOAuthRedirect', from: '@src/auth/hooks.js' },
});

app.server({
  setupFn: { importDefault: 'mySetupFunction', from: '@src/server/myServerSetupCode.js' },
});

app.client({
  setupFn: { importDefault: 'myClientSetupFunction', from: '@src/client/myClientSetupCode.js' },
  rootComponent: { importDefault: 'App', from: '@src/client/App.jsx' }
});

app.emailSender({
  provider: 'SendGrid',
  defaultFrom: { name: 'Hello', email: 'hello@itsme.com' }
});

const mainPage = app.page('MainPage', {
  component: { import: 'MainPage', from: '@src/MainPage' }
});
app.route('RootRoute', { path: '/', to: mainPage });

app.job('mySpecialJob', {
  executor: 'PgBoss',
  perform: {
    fn: { import: 'foo', from: '@src/server/jobs/bar.js' }
  }
});

app.job('returnHelloJob', {
  executor: 'PgBoss',
  perform: {
    fn: { import: 'returnHello', from: '@src/server/jobs/returnHello.js' }
  },
  entities: ['User']
});

app.action('mySpecialAction', {
  fn: { import: 'foo', from: '@src/server/actions/bar.js' },
  entities: ['User']
});

app.query('mySpecialQuery', {
  fn: { import: 'foo', from: '@src/server/queries/bar.js' },
  entities: ['User']
});

app.apiNamespace('fooBarNamespace', {
  middlewareConfigFn: { import: 'fooBarNamespaceMiddlewareFn', from: '@src/server/apiNamespaces.js' },
  path: '/bar'
});

app.api('fooBar', {
  fn: { import: 'fooBar', from: '@src/server/apis.js' },
  httpRoute: ['GET', '/foo/bar'],
  middlewareConfigFn: { import: 'fooBarMiddlewareFn', from: '@src/server/apis.js' }
});

app.api('fooBaz', {
  fn: { import: 'fooBaz', from: '@src/server/apis.js' },
  httpRoute: ['GET', '/foo/baz'],
  auth: false
});

app.crud('tasks', {
  entity: 'Task',
  operations: {
    get: {},
    getAll: {},
    create: {}
  }
});

export default app;
