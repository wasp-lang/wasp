<!--  Use for both Railway and Fly intro sections in the cli.md --->
When you run the `launch` command, Wasp CLI knows how to connect different parts of your Wasp app together, so it sets up the required environment variables for your server app:

1. `WASP_WEB_CLIENT_URL` and `WASP_SERVER_URL` which are required to connect your client and server apps.
1. `DATABASE_URL` which is required to connect your server app to the database.
1. `JWT_SECRET` which is required for authentication to work.