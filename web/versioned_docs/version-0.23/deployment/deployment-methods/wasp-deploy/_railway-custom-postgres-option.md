import CodeBlock from '@theme/CodeBlock'

#### Using a custom PostgreSQL database

By default, Wasp uses the standard PostgreSQL image provided by Railway when creating a new database for your app. However, if your application requires specific PostgreSQL extensions (e.g., PostGIS), you can specify a Docker image with a custom PostgreSQL installation, with the `--db-image <docker-image>` flag.

:::tip
You only need to specify the Docker image once, when first creating the app.
:::

<CodeBlock language="shell">{
`# Use PostGIS:
wasp deploy railway ${props.command} my-wasp-app --db-image postgis/postgis
`
}</CodeBlock>

<CodeBlock language="shell">{
`# Use pgvector:
wasp deploy railway ${props.command} my-wasp-app --db-image pgvector/pgvector:pg16
`
}</CodeBlock>

The service name will always be `Postgres`, regardless of the image used.
