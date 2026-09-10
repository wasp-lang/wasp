<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

The redirect URL is your **app's** URL, since the server serves the client and the browser talks to one origin. In development that's `http://localhost:3000`, where the client dev server proxies Wasp's auth routes to the server.

</TabItem>
<TabItem value="split" label="Split deployment">

The redirect URL is your **server's** URL, which is a different origin than the client. In development that's `http://localhost:3001`, and in production the server's public origin.

</TabItem>
</Tabs>
