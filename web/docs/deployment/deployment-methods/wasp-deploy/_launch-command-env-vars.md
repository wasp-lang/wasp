<!--  Use for both Railway and Fly intro sections in the cli.md --->
When you run the `launch` command, Wasp CLI knows what your Wasp app needs to run, so it sets up the required environment variables for it:

1. `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL`, both set to your app's own URL, since one app serves both your pages and your API.
1. `DATABASE_URL` which is required to connect your app to the database.
1. `JWT_SECRET` which is required for authentication to work.