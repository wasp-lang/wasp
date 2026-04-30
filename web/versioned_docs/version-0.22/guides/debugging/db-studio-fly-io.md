---
title: Database Studio with Fly.io
last_checked_with_versions:
  Wasp: 0.21.1
  Fly CLI: 0.4.11
comments: true
---

This guide shows you how to connect to your production database on Fly.io and run `wasp db studio` to inspect or modify your data.

## Prerequisites

- A Wasp app deployed to [Fly.io](https://fly.io/)
- The [Fly CLI](https://fly.io/docs/hands-on/install-flyctl/) installed and authenticated

## Overview

To connect to your production database, you'll need to:

1. Get the database name
2. Get the database password
3. Open a tunnel to the database
4. Configure your local environment
5. Run `wasp db studio`

## Step 1: Get the Database Name

Connect to your Postgres app (replace `some-test-db` with your actual database app name):

```bash
fly postgres connect -a some-test-db
```

Once connected, list all databases:

```sql
\l
```

Your database name will typically follow the pattern `server_name_with_underscores`. For example, if your server app is named `some-test-server`, the database name would be `some_test_server`.

Type `\q` to exit the Postgres prompt.

## Step 2: Get the Database Password

SSH into your database app:

```bash
fly ssh console -a some-test-db
```

Then retrieve the password:

```bash
echo $OPERATOR_PASSWORD
```

Copy this password and type `exit` to leave the SSH session.

## Step 3: Open a Database Tunnel

Before opening the tunnel, make sure nothing else is running on port 5432:

- Stop any local database started with `wasp db start`
- Check for Docker containers that might be using the port

:::caution
Even if you close the terminal that was running `wasp db start`, the Docker container may still be running in the background. Make sure to stop it before proceeding.
:::

Open the tunnel:

```bash
fly proxy 5432 -a some-test-db
```

Keep this terminal tab open and use a new terminal for the following steps.

## Step 4: Configure the Database URL

Edit your `.env.server` file to point to the production database:

```bash title=".env.server"
DATABASE_URL=postgres://postgres:<password>@localhost:5432/<db_name>
```

Replace `<password>` with the password from Step 2 and `<db_name>` with the database name from Step 1.

For example:

```bash title=".env.server"
DATABASE_URL=postgres://postgres:myDatabasePassword@localhost:5432/some_test_server
```

## Step 5: Run Database Studio

Now you can run Prisma Studio to browse and edit your production data:

```bash
wasp db studio
```

This will open a web interface where you can view and modify your database records.

## Security Considerations

:::warning
Be careful when modifying production data. Always back up your database before making changes, and consider using a read-only user for routine inspections.
:::

- Remember to restore your `.env.server` to point to your local database when you're done
- Close the tunnel when finished by terminating the `fly proxy` command
- Never commit production credentials to version control
