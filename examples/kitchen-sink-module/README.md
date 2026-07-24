<!-- prettier-ignore -->
# @kitchen-sink/module

Build:

```sh
wasp module install
wasp module build
```

Pack and install into Kitchen Sink after building:

```sh
node ../../scripts/pack-preview.ts . ../kitchen-sink src/modules
cd ../kitchen-sink
../../waspc/run wasp-cli clean
../../waspc/run wasp-cli install
```

Import from a Wasp app:

```ts
import getModuleSpec from "@kitchen-sink/module/spec";
```
