import { createStripePaymentsModule } from "@waspello/stripe-payments";
import { createTodoModule } from "@waspello/todo-module";
import { ActionConfig, App, ExtImport } from "wasp-config";

const app = new App("waspello", {
  title: "Waspello",
  wasp: { version: "^0.21.2" },
});

app.client({
  rootComponent: { importDefault: "Layout", from: "@src/Layout" },
});

app.emailSender({ provider: "Dummy" });

app.auth({
  userEntity: "User",
  methods: {
    email: {
      userSignupFields: {
        import: "getEmailUserFields",
        from: "@src/auth/userSignupFields",
      },
      fromField: {
        name: "Waspello",
        email: "noreply@waspello.dev",
      },
      emailVerification: {
        clientRoute: "EmailVerificationRoute",
      },
      passwordReset: {
        clientRoute: "PasswordResetRoute",
      },
    },
  },
  onAuthFailedRedirectTo: "/login",
});

app.route("EmailVerificationRoute", {
  path: "/email-verification",
  to: app.page("EmailVerification", {
    component: {
      importDefault: "EmailVerification",
      from: "@src/auth/EmailVerificationPage",
    },
  }),
});

app.route("PasswordResetRoute", {
  path: "/password-reset",
  to: app.page("PasswordReset", {
    component: {
      importDefault: "PasswordReset",
      from: "@src/auth/PasswordResetPage",
    },
  }),
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

app.use(
  createStripePaymentsModule({
    userEntityName: "User",
    // TODO: Support reading env vars at runtime instead of hardcoding
    premiumPlanPriceId: "price_1T4oHFFaHKs7M0PTaaRNXrj6",
    subscriptionRoute: "/subscription",
  }),
);

app.use(
  createTodoModule({
    todoEntityName: "TodoItem",
    route: "/todos",
    cleanDoneTodosCron: "* * * * *",
    userEntityName: "User",
    userForeignKey: "userId",
  }),
);

export default app;
