import { app, page, route } from "@wasp.sh/spec";

import { HomePage } from "./src/pages/HomePage";

export default app({
  name: "AppWithNeither",
  wasp: { version: "0.25.0" },
  spec: [route("HomeRoute", "/", page(HomePage))],
});
