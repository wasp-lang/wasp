# Waspello

**Waspello** is a Trello-style kanban board app built with [Wasp](https://wasp.sh). It demonstrates how a moderately complex, multi-user real-time app can be built with very little boilerplate — most of the auth, queries, and actions are declared in a single `main.wasp.ts` file.

This app is deployed on Netlify at [https://waspello-demo.netlify.app/](https://waspello-demo.netlify.app/).

The backend is hosted on Fly.io at https://waspello.fly.dev.

## Features

- Email / password authentication
- Multiple boards, each with lists and cards
- Real-time updates across users (queries invalidate on mutation)
- Drag-and-drop reordering of lists and cards
- Image attachments on cards

## Development

Run `npm install` before using the project. It also installs Wasp, which you run through npm.

### Database

Wasp needs the Postgres database running.

Easiest way to do this is to use `npm run wasp -- start db` to start a PostgreSQL locally using Docker.
Then run `npm run wasp -- db migrate-dev` in another terminal.

### Env variables

Copy `env.server` to `.env.server` and fill in the values.

### Running

`npm run dev`

### Installing with the development version of Wasp

<!-- TODO(#2262): Remove this section once the examples join the pnpm workspace. -->

In this repo, the example depends on the development version of Wasp, which isn't on npm, so `npm install` fails. Build Wasp from source instead (see [`waspc/README.md`](../../waspc/README.md)), and then install against it from this directory:

```sh
mise run build
mise run reinstall-dev-wasp-app-deps
```

## Testing

### End-to-end tests

Waspello uses Playwright for e2e tests. Run them with:

```sh
npm run test:e2e
```

The e2e suite lives in `e2e-tests/` and is also what CI runs on every PR.
