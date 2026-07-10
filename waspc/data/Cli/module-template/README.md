<!-- prettier-ignore -->
# __waspModulePackageName__

Build:

```sh
wasp module install
wasp module build
```

Pack for installing into a host app:

```sh
npm run pack
```

Import from a Wasp app:

```ts
import getModuleSpec from "__waspModulePackageName__/spec";
```
