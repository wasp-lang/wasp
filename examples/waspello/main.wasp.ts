import { ActionConfig, App, ExtImport } from "wasp-config";

const app = new App("waspello", {
  title: "Waspello",
  wasp: { version: "^0.18.0" },
});

app.auth({
  userEntity: "User",
  methods: {
    usernameAndPassword: {},
    google: {},
  },
  onAuthFailedRedirectTo: "/login",
});

/* Pages */

// You can define pages and routes separately...
const mainPage = app.page("Main", {
  authRequired: true,
  component: {
    importDefault: "Main",
    from: "@src/cards/MainPage",
  },
});

app.route("MainRoute", { path: "/", to: mainPage });

const signupPage = app.page("Signup", {
  component: {
    importDefault: "Signup",
    from: "@src/auth/SignupPage",
  },
});
app.route("SignupRoute", { path: "/signup", to: signupPage });

// ... Or define them together
app.route("LoginRoute", {
  path: "/login",
  to: app.page("Login", {
    component: {
      importDefault: "Login",
      from: "@src/auth/LoginPage",
    },
  }),
});

/* Operations */

// You can define them them one by one...
app.query("getListsAndCards", {
  fn: { import: "getListsAndCards", from: "@src/cards/lists" },
  entities: ["List", "Card"],
});

app.action("createList", {
  fn: { import: "createList", from: "@src/cards/lists" },
  entities: ["List"],
});

// ...Or, if they're similar enough, create a helper function to reduce
// duplication:
function appAction(
  name: string,
  from: ExtImport["from"],
  entities: ActionConfig["entities"],
) {
  app.action(name, {
    fn: { import: name, from },
    entities,
  });
}

appAction("updateList", "@src/cards/lists", ["List"]);
appAction("createCard", "@src/cards/cards", ["Card"]);
appAction("updateCard", "@src/cards/cards", ["Card"]);
appAction("deleteList", "@src/cards/lists", ["List", "Card"]);
appAction("createListCopy", "@src/cards/lists", ["List", "Card"]);

export default app;
