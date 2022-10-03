Waspello 
=========

**Waspello** is a trello clone app made with Wasp.

This app is deployed at [https://waspello.netlify.app/](https://waspello.netlify.app/).

# Development

### Databse
Wasp needs postgre database running - provide it with database conncetion URL via env var `DATABASE_URL` - best to it via `.env` file.

Easy way to get going with postgresql database:
- run db with 
```sh
docker run --rm --publish 5432:5432 -v postgresql-data:/var/lib/postgresql/data --env POSTGRES_PASSWORD=devpass postgres
```
- Add `DATABASE_URL` to the `.env` file.
`DATABASE_URL` in this case is `postgresql://postgres:devpass@localhost:5432/postgres`.
example:
```sh
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```

### Running
`wasp start`
