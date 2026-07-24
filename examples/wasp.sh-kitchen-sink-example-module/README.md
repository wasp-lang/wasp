<!-- prettier-ignore -->
# @wasp.sh/kitchen-sink-example-module

Experimental example package for an unreleased Wasp version. Not intended for
production use.

It contributes a route, page, query, action, CRUD, API, API namespace, and job.
It demonstrates distributing a full-stack Wasp module through npm with client
and server code, CSS, an npm dependency, and host app integration.

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

## Development

Build the module and activate a temporary local tarball in Kitchen Sink:

```sh
../../waspc/run wasp-cli module install
../../waspc/run wasp-cli module build
node ../../scripts/pack-preview.ts . ../kitchen-sink src/modules
cd ../kitchen-sink
../../waspc/run wasp-cli clean
../../waspc/run wasp-cli install
```

The tarball is ignored by Git. Restore the published dependency before
committing.
