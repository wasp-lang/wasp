This package provides a command-line interface for getting information about
exported symbols from JS/TS files. As input you give it a list of exports requests,
each containing a list of filepaths and, optionally, a path to a tsconfig file.

```json
[
  { "filenames": ["./src/exports.ts"] },
  {
    "tsconfig": "~/dev/wasp-todoapp/src/client/tsconfig.json",
    "filenames": ["~/dev/wasp-todoapp/src/client/MainPage.tsx"]
  }
]
```

Note that an instance of the TypeScript compiler is created for each exports
request, so grouping all files with the same tsconfig into one request confers
some performance benefit.

The program responds with a list of exports for each file. The filepaths in the
result will always exactly match the filepaths in the input:

```json
{
  "./src/exports.ts": [
    { "type": "named", "name": "getExportsOfFiles" },
    { "type": "default" }
  ]
}
```
