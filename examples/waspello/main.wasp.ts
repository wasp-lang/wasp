import { App } from 'wasp-config'

const app = new App('waspello', {
  title: 'Waspello',
  wasp: { version: '^0.16.0' },
});

app.auth({
  userEntity: 'User',
  methods: {
    usernameAndPassword: {},
    google: {}
  },
  onAuthFailedRedirectTo: '/login'
});

/* Pages */

// You can define pages and routes separately...
const mainPage = app.page('Main', {
  authRequired: true,
  component: {
    importDefault: 'Main',
    from: '@src/cards/MainPage.jsx'
  }
});

app.route('MainRoute', { path: '/', to: mainPage });

const signupPage = app.page('Signup', {
  component: {
    importDefault: 'Signup',
    from: '@src/auth/SignupPage.jsx'
  }
});
app.route('SignupRoute', { path: '/signup', to: signupPage });

// ... Or define them together
app.route('LoginRoute', {
  path: '/login',
  to: app.page('Login', {
    component: {
      importDefault: 'Login',
      from: '@src/auth/LoginPage.jsx'
    }
  })
});

/* Operations */

app.query('getListsAndCards', {
  fn: { import: 'getListsAndCards', from: '@src/cards/lists.js' },
  entities: ['List', 'Card']
});

app.action('createList', {
  fn: { import: 'createList', from: '@src/cards/lists.js' },
  entities: ['List']
});

app.action('updateList', {
  fn: { import: 'updateList', from: '@src/cards/lists.js' },
  entities: ['List']
});

app.action('deleteList', {
  fn: { import: 'deleteList', from: '@src/cards/lists.js' },
  entities: ['List', 'Card']
});

app.action('createListCopy', {
  fn: { import: 'createListCopy', from: '@src/cards/lists.js' },
  entities: ['List', 'Card']
});

app.action('createCard', {
  fn: { import: 'createCard', from: '@src/cards/cards.js' },
  entities: ['Card']
});

app.action('updateCard', {
  fn: { import: 'updateCard', from: '@src/cards/cards.js' },
  entities: ['Card']
});

export default app;
