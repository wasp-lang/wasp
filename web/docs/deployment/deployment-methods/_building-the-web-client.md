To build the web app, position yourself in `.wasp/build/web-app` directory:

```
cd .wasp/build/web-app
```

Run

```
npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
```

where `<url_to_wasp_backend>` is the URL of the Wasp server that you previously deployed.

:::caution Client Env Variables
Remember, if you have defined any other [client-side env variables](../../project/env-vars.md#defining-env-vars-in-development) in your project, make sure to add them to the command above when [building your client](../env-vars.md#client-env-vars)
:::
