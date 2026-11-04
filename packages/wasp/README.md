# Wasp

Wasp is a Rails-like framework for React, Node.js, and Prisma. You describe your app's pages, routes, auth, and operations in `main.wasp.ts`, and you write the rest in React and Node.js. Wasp generates the code in between, like full-stack auth, type-safe calls from the client to the server, background jobs, and email sending. You deploy the whole app with a single command.

Your Wasp app depends on this package, and the version of this package sets the Wasp version your app runs.

## Create your first app

Create a new app and start it:

```sh
npx @wasp/cli@latest new
cd <my-project-name>
npm run dev
```

## Learn more

- Website: https://wasp.sh/
- Tutorial: https://wasp.sh/docs/tutorial/create
- Discord community: https://discord.gg/rzdnErX
