To build the web app, position yourself in `.wasp/build/web-app` directory:

```
cd .wasp/build/web-app
```

Run

```
npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
```

where `<url_to_wasp_backend>` is the URL of the Wasp server that you previously deployed.
