# Waspello

**Waspello** is a trello clone app made with Wasp.

This app is deployed on Netlify at [https://waspello-demo.netlify.app/](https://waspello-demo.netlify.app/).

The backend is hosted on Fly.io at https://waspello.fly.dev.

# Development

This app uses Wasp's new TS spec. You will have to run `wasp ts-setup` before using the project.
Check the docs for full instructions on using the Wasp TS Spec: https://wasp.sh/docs/general/wasp-ts-config.

### Database

Wasp needs the Postgres database running.

Easiest way to do this is to use `wasp start db` to start a PostgreSQL locally using Docker.

### Env variables

Copy `env.server` to `.env.server` and fill in the values.

### Running

`wasp start`
