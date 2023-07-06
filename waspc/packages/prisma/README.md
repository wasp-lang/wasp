This package contains useful tools for working with Prisma, especially PSL.

## Setup
Run `npm install`.

## Usage

Run `npm run build` to compile TS to JS. Do this after any changes to TS files, or if you have never run it before.

### format
Run `npm start format` and pass prisma schema source (so PSL) via stdin.
It can even be an incomplete schema -> e.g. just model declarations.

This will run `prisma format` on it and return formatted schema (PSL) + any warnings/errors.
Note: `prisma format` does not just format stuff, but even fix some simple mistakes like incomplete relations!

Response, on stdout, will be JSON of the following shape:
```json
{
  "formattedSchemaPsl": "<psl>",
  "errors": "<all errors as one big string>"  // undefined if no errors.
}
```
