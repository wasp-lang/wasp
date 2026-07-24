<!-- prettier-ignore -->
# @wasp.sh/kitchen-sink-example-module

This is an experimental example for an unreleased Wasp version. Do not use it
in production.

It adds a route, page, query, action, CRUD, API, API namespace, and job to a Wasp
app. It shows that a module published to npm can include client and server code,
CSS, its own npm dependencies, and code that uses the app's auth and database.

Install it and add it to the app's spec:

```sh
npm install @wasp.sh/kitchen-sink-example-module
```

```ts
import { app } from "@wasp.sh/spec";
import getModuleSpec from "@wasp.sh/kitchen-sink-example-module/spec";

export default app({
  // ...
  spec: [getModuleSpec({ prefix: "/example" })],
});
```

## Module contract

The app must:

- Use PostgreSQL because the module runs a PgBoss job.
- Configure auth with a `User` model.
- Give `User` a numeric ID and a `tasks Task[]` relation.
- Give `Task` a numeric ID, `description`, `isDone`, and a required `user`
  relation.
- Avoid names already used by the module.

## Development

To test local changes in Kitchen Sink:

```sh
../../waspc/run wasp-cli module install
../../waspc/run wasp-cli module build
node ../../scripts/pack-preview.ts . ../kitchen-sink src/modules
cd ../kitchen-sink
../../waspc/run wasp-cli clean
../../waspc/run wasp-cli install
```

The script creates a Git-ignored tarball and temporarily points Kitchen Sink at
it. Before committing, publish a new version and point Kitchen Sink back to the
published package.
