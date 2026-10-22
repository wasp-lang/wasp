:::caution Setting the correct client URL

If you set the `baseDir` option, make sure that your client URL also includes that base directory. In production, that is the `WASP_WEB_CLIENT_URL` env variable. In development, Wasp adds the base directory for you, unless you pass a custom URL with `--client-url`.

For example, if you are serving your app from `https://example.com/my-app`, the client URL should also be `https://example.com/my-app`, and not just `https://example.com`.
:::
