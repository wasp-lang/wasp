<!-- prettier-ignore -->
# @kitchen-sink/module

Build:

```sh
wasp module install
wasp module build
```

Pack for installing into a host app (the tarball must live inside the host
project under `src/`, e.g. Kitchen Sink's `src/modules/` dir):

```sh
npm run pack
cp kitchen-sink-module-0.0.1.tgz ../kitchen-sink/src/modules/
```

Import from a Wasp app:

```ts
import getModuleSpec from "@kitchen-sink/module/spec";
```
