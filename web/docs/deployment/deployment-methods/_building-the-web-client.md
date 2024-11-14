To build the web app, position yourself in `.wasp/build/web-app` directory:

```
cd .wasp/build/web-app
```

Run

```
npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
```

where `<url_to_wasp_backend>` is the URL of the Wasp server that you previously deployed.

:::caution Client Environment Variables
Remember, if you have manually defined any other [client-side environment variables](../../project/env-vars#client-env-vars) in your project, make sure to add them to the command above when [building your client](../../project/env-vars#client-env-vars-1)
:::