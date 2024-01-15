Waspello
=========

**Waspello** is a trello clone app made with Wasp.

This app is deployed on Netlify at [https://waspello-demo.netlify.app/](https://waspello-demo.netlify.app/).

The backend is hosted on Fly.io at https://waspello.fly.dev.

# Development

### Database
Wasp needs the Postgres database running. 

Easiest way to do this is to use `wasp start db` to start a PostgreSQL locally using Docker.

### Env variables
Copy `env.server` to `.env.server` and fill in the values.

### Running

`wasp start`
