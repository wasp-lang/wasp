import CodeBlock from '@theme/CodeBlock'

#### Using a custom PostgreSQL database

Wasp creates new Railway databases with PostgreSQL 18, the same major version Wasp uses during development. It uses the [`ghcr.io/railwayapp-templates/postgres-ssl:18`](https://github.com/railwayapp-templates/postgres-ssl) image by default. Railway stores the database data in a [volume](https://docs.railway.com/volumes).

Use `--db-image <docker-image>` if your app needs another PostgreSQL image, for example, to add PostGIS. The default image already includes pgvector. If the custom image stores data at a different location, set it with `--db-volume-mount-path <path>`. It defaults to `/var/lib/postgresql/data`.

:::tip
You only need to specify the Docker image once, when first creating the app.
:::

<CodeBlock language="shell">{
`# Use PostGIS:
wasp deploy railway ${props.command} my-wasp-app --db-image postgis/postgis:18-3.6
`
}</CodeBlock>

Railway's [Database View](https://docs.railway.com/databases/database-view) isn't available for databases created by these commands. You can instead [connect `wasp db studio` to the production database](../../database.md#connect-to-the-production-database).
