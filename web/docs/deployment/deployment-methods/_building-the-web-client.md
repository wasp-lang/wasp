To build the web client for hosting it separately from the server, run the following command from your project root after `wasp build`:

```
REACT_APP_API_URL=<url_to_wasp_backend> npx vite build
```

where `<url_to_wasp_backend>` is the origin of the Wasp server that you previously deployed (for example `https://api.myapp.com`).

The build output will be in `.wasp/out/web-app/build`.

:::caution Client Env Variables
Remember, if you have defined any other [client-side env variables](/docs/project/env-vars#defining-env-vars-in-development) in your project, make sure to add them to the command above when [building your client](/docs/deployment/env-vars#client-env-vars)
:::
