import { App } from 'wasp-config'

const app = new App('Thoughts', {
  title: 'Thoughts',
  wasp: { version: '^0.15.0' }
});

app.auth({
  userEntity: 'User',
  methods: {
    usernameAndPassword: {}
  },
  onAuthFailedRedirectTo: '/login'
});

app.db({
  seeds: [
    { import: 'devSeedBasic', from: '@src/server/seeds.js' }
  ]
});

const mainPage = app.page('MainPage', {
  component: { importDefault: 'Main', from: '@src/client/MainPage.jsx' },
  authRequired: true
});
app.route('MainRoute', { path: '/', to: mainPage });

const thoughtsPage = app.page('ThoughtsPage', {
  component: { importDefault: 'Thoughts', from: '@src/client/ThoughtsPage.jsx' },
  authRequired: true
});
app.route('ThoughtsRoute', { path: '/thoughts', to: thoughtsPage });

const loginPage = app.page('LoginPage', {
  component: { importDefault: 'Login', from: '@src/client/LoginPage.jsx' }
});
app.route('LoginRoute', { path: '/login', to: loginPage });

const signupPage = app.page('SignupPage', {
  component: { importDefault: 'Signup', from: '@src/client/SignupPage.jsx' }
});
app.route('SignupRoute', { path: '/signup', to: signupPage });

app.action('createThought', {
  fn: { import: 'createThought', from: '@src/server/actions.js' },
  entities: ['Thought', 'Tag']
});

app.query('getThoughts', {
  fn: { import: 'getThoughts', from: '@src/server/queries.js' },
  entities: ['Thought']
});

app.query('getTags', {
  fn: { import: 'getTags', from: '@src/server/queries.js' },
  entities: ['Tag']
});

export default app;
