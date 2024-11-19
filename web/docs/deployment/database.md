---
title: Database
---

In this section, we'll discuss what happens with the database when your app goes live. When you develop your app locally, you probably use the dev database that you start with the `wasp db start` command. However, when it's time to deploy your app, you'll need to set up a production database.

### Production database requirements

The server app that Wasp generates uses a PostgreSQL database. The only requirement from Wasp's point of view is that the database is accessible from the server via the `DATABASE_URL` environment variable.

It can be a PostgreSQL database running on the same server as the server app, or it can be a managed PostgreSQL database service like [Fly Postgres](https://fly.io/docs/postgres/), [AWS RDS](https://aws.amazon.com/rds/), or some other service.

## Migrations

Every time you make a change in your [Prisma schema](../data-model/prisma-file.md) e.g. adding a new model, changing a field type, etc., you need to create a migration. Migrations are some code that describes the change you made in the schema, and they are used to apply the change to the database.

The benefit of migrations is that you can apply the same change to multiple databases, e.g. your local development database and your production database. 

### Creating migrations

After you made a change in the Prisma schema, you can create a migration by running the following command:

```bash
wasp db migrate-dev
```

This command will create a new migration in the `migrations` directory. The migration is a set of SQL commands that describe the change you made in the schema.

### Applying migrations

**In development**, the migrations are applied as soon as you run the `wasp start` command. 

**In production**, the server app first checks if there are any new migrations that need to be applied, and if there are, it applies them before starting the server. This way, the database schema is always in sync with the Prisma schema.

The migrations might fail to apply if there is a conflict with the existing data in the database. In that case, you'll need to fix the migration and try again.

### Debugging failed migrations

If a migration fails to apply, the server app will log the error message and stop. You should then connect to the production database and see what went wrong. If you check the `_prisma_migrations` table, you'll see the failed migration there. 

You can try resolving the erorr e.g. if you tried adding a `@unique` constraint to a field that already has duplicate values:
1. Remove any duplicate values from the database
2. Remove the failed migration from the `_prisma_migrations` table
3. Try applying the migration again by restarting the server app

:::tip Viewing the `_prisma_migrations` table

You can't use the `wasp db studio` command to view the `_prisma_migrations` table in the production database, but you can use a database management tool like [DBeaver](https://dbeaver.io/) or [pgAdmin](https://www.pgadmin.org/).
:::

## Connect to the production database

**In development**, you can use the `wasp db studio` command to open a web-based database management tool that allows you to inspect the database. 

You can use the same tool to inspect the **production database**, but you'll need to set the `DATABASE_URL` environment variable to point to the production database. Set the variable in the `.env.server` file in the root of your project.

```bash title=".env.server"
DATABASE_URL="postgresql://user:password@host:port/dbname"
```

Then you can start the database studio with the following command:

```bash
wasp db studio
```

If you are looking how to connect to a Fly.io production database, we wrote a [guide](https://github.com/wasp-lang/learning-materials/?tab=readme-ov-file#running-wasp-db-studio-on-production-db) on how to do that.