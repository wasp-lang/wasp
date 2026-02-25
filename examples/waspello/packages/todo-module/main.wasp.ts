import { App } from "wasp-config";
import { createTodoModule } from "./src/index.js";

const app = new App("todoModuleDev", {
  title: "Todo Module Dev",
  wasp: { version: "^0.21.2" },
});

app.auth({
  userEntity: "User",
  methods: {
    usernameAndPassword: {},
  },
  onAuthFailedRedirectTo: "/login",
});

const loginPage = app.page("LoginPage", {
  component: { import: "LoginPage", from: "@src/scaffold/LoginPage" },
});
app.route("LoginRoute", { path: "/login", to: loginPage });

const signupPage = app.page("SignupPage", {
  component: { import: "SignupPage", from: "@src/scaffold/SignupPage" },
});
app.route("SignupRoute", { path: "/signup", to: signupPage });

app.use(
  createTodoModule({
    todoEntityName: "TodoItem",
    route: "/",
    userEntityName: "User",
    userForeignKey: "userId",
  }),
);

export default app;
