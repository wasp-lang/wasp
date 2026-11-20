# Wasp Studio Client

It's a React + Vite app that reads the app's spec from `/__wasp/spec` and shows it as a graph. It reads it from the server that serves the page, or from the one in the `serverUrl` query parameter.

### Development

Run `wasp start` in a Wasp app, and then run:

```
pnpm run dev
```

Then open the URL Vite prints, with `?serverUrl=http://localhost:3001`.
