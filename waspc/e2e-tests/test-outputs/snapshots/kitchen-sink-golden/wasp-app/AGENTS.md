## Wasp

This project uses the Wasp framework.

### Docs

- Find docs via the index at https://wasp.sh/llms.txt
- Pick the docs map matching the version of the `@wasp/wasp` dependency in `package.json`.
- Prefer raw Markdown URLs from the docs map over guessing `wasp.sh/docs` URLs.
- If this file conflicts with versioned Wasp docs, trust the docs and tell the user this file may be stale.

### Wasp TypeScript Spec

- `main.wasp.ts` contains the Wasp app specification.
- When it references app components/functions, import them from `src/` with `with { type: "ref" }`.
- `route(name, ...)` and `crud(name, ...)` take an explicit name.
- Other constructors take no name argument; the declaration's name is the imported identifier. For example, `job(sendReminder, { ... })` declares a job named `sendReminder`.

### Running Wasp

- Prefer the `package.json` scripts, which run the project's Wasp version: `npm run dev` (`wasp start`), `npm run build` (`wasp build`), `npm run preview` (`wasp build start`), and `npm run wasp -- <command>` for any other Wasp command.
- Don't use `npx wasp`: in a fresh clone, it downloads an unrelated npm package named `wasp`.

### Fresh checkout

- Run `npm install` before other Wasp commands in a fresh clone or worktree.
- Set up `.env.server` and `.env.client` from the example files or the project README.
- Run `npm run wasp -- db migrate-dev`. Check the project README to see whether you also need to run a seed.

### Verification

- Run `npm run wasp -- compile` to check if app is valid.
- Do not run `tsc` directly for validation.
