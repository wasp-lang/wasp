import { app, page, route } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };

export default app({
  name: "__waspAppName__",
  wasp: { version: "__waspVersion__" },
  title: "__waspProjectName__",
  head: [<link rel="icon" href="/favicon.ico" />],
  spec: [
    route("RootRoute", "/", page(MainPage)),
  ],
});
