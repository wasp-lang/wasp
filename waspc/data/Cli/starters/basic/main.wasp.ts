import { app } from "@wasp.sh/spec";
import { App } from "./src/App" with { type: "ref" };
import { auth, authParts } from "./src/auth/auth.wasp";
import { tagsParts } from "./src/tags/tags.wasp";
import { tasksParts } from "./src/tasks/task.wasp";

export default app({
  name: "__waspAppName__",
  title: "__waspProjectName__",
  wasp: { version: "__waspVersion__" },
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth,
  emailSender: {
    provider: "Dummy",
  },
  client: {
    rootComponent: App,
  },
  parts: [...authParts, ...tasksParts, ...tagsParts],
});
