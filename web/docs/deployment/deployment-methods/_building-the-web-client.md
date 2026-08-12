Your app's pages and assets are built together with the rest of your app, inside the Docker image. That is also where your [client env variables](/docs/deployment/env-vars#client-env-vars) have to be, since their values end up inside the built files.

You pass them to the build with the `WASP_CLIENT_ENV` build argument, as shell assignments, one per line:

```shell
docker build \
  --build-arg WASP_CLIENT_ENV="REACT_APP_SOME_VAR_NAME='somevalue'" \
  -t my-wasp-app \
  .wasp/out
```

If your app doesn't have any client env variables, you can leave the argument out.
