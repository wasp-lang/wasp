NOTE: `typescript` is purposefully a normal dependency instead of a dev
dependency.

Run the program `node ./dist/index.js` and pass a list of export requests over
stdin:

```json
[
  { "filenames": ["./src/exports.ts"] },
  {
    "tsconfig": "~/dev/wasp-todoapp/src/client/tsconfig.json",
    "filenames": "~/dev/wasp-todoapp/src/client/MainPage.tsx"
  }
]
```

It will respond with an object mapping filenames to exports, something like:

```json
{
  "./src/exports.ts": [
    { "type": "named", "name": "getExportsOfFiles" },
    { "type": "default" }
  ]
}
```
