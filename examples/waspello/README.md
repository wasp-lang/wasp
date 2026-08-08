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

Run `wasp install` before using the project.

### Database

Wasp needs the Postgres database running.

Easiest way to do this is to use `wasp start db` to start a PostgreSQL locally using Docker.
Then run `wasp db migrate-dev` in another terminal.

### Env variables

Copy `env.server` to `.env.server` and fill in the values.

### Running

`wasp start`

## Testing

### End-to-end tests

Waspello uses Playwright for e2e tests. Run them with:

```sh
npm run test
```

The e2e suite lives in `e2e-tests/` and is also what CI runs on every PR.
