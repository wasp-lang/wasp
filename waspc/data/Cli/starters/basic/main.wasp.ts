import { app } from "@wasp.sh/spec";
import { App } from "./src/App" with { type: "ref" };
import { auth, authConfig } from "./src/auth/auth.wasp";
import { tags } from "./src/tags/tags.wasp";
import { tasks } from "./src/tasks/task.wasp";

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
  decls: [...auth, ...tasks, ...tags],
});
