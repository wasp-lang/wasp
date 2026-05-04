import { App } from "wasp-config";

const app = new App("__waspAppName__", {
  title: "__waspProjectName__",
  wasp: { version: "__waspVersion__" },
  head: ["<link rel='icon' href='/favicon.ico' />"],
});

const mainPage = app.page("MainPage", {
  component: { import: "MainPage", from: "@src/MainPage" },
});

app.route("RootRoute", { path: "/", to: mainPage });

export default app;
