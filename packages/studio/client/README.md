# Wasp Studio Client

It's a React + Vite app that reads the app's spec from the server that `wasp start` runs (`/__wasp/spec`), and shows it as a graph.

### Development

Install deps:

```
pnpm install
```

Run `wasp start` in a Wasp app, and then run:

```
pnpm run dev
```

The page reads from the server at `http://localhost:3001`. Pass `?serverUrl=<url>` to use another one.
