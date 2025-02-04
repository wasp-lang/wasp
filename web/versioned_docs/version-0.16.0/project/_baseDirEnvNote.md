:::caution Setting the correct env variable

If you set the `baseDir` option, make sure that the `WASP_WEB_CLIENT_URL` env variable also includes that base directory.

For example, if you are serving your app from `https://example.com/my-app`, the `WASP_WEB_CLIENT_URL` should be also set to `https://example.com/my-app`, and not just `https://example.com`.
:::