import { app } from "@wasp.sh/spec";
import { App } from "./src/App" with { type: "ref" };
import { authConfig, auth as authDecls } from "./src/auth/auth.wasp";
import { tagsDecls } from "./src/tags/tags.wasp";
import { tasksDecls } from "./src/tasks/task.wasp";

export default app({
  name: "__waspAppName__",
  title: "__waspProjectName__",
  wasp: { version: "__waspVersion__" },
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: authConfig,
  emailSender: {
    provider: "Dummy",
  },
  client: {
    rootComponent: App,
  },
  decls: [...authDecls, ...tasksDecls, ...tagsDecls],
});
