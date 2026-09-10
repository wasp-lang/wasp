<!--  Use for both Railway and Fly intro sections in the cli.md --->
When you run the `launch` command, Wasp CLI knows how to connect different parts of your Wasp app together, so it sets up the required environment variables for your server app:

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

1. `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL`, both set to your app's URL, since the server also serves the client.
1. `DATABASE_URL` which is required to connect your server app to the database.
1. `JWT_SECRET` which is required for authentication to work.

If you use OAuth, register `<app URL>/auth/<provider>/callback` (for example `https://my-wasp-app-server.fly.dev/auth/google/callback`) as the redirect URI with your provider.

</TabItem>
<TabItem value="split" label="Split deployment">

1. `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL`, set to the server's and the client's URLs, which is what connects your client and server apps.
1. `DATABASE_URL` which is required to connect your server app to the database.
1. `JWT_SECRET` which is required for authentication to work.

If you use OAuth, register `<server URL>/auth/<provider>/callback` as the redirect URI with your provider.

</TabItem>
</Tabs>
