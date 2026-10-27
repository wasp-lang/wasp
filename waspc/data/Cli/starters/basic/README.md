# Basic Starter – A Simple ToDo App

Basic starter is a well-rounded template that showcases the most important bits of working with Wasp.

## Prerequisites

- **Node.js** (newest LTS version recommended): We recommend install Node through a Node version manager, e.g. `nvm`.
- **Wasp** (latest version): Install via
  ```sh
  npm i -g @wasp/cli@latest
  ```

## Using the template

You can use this template through the Wasp CLI:

```bash
wasp new <project-name>
# or
wasp new <project-name> -t basic
```

## Development

After cloning the project, run `wasp install`.

To start the application locally for development or preview purposes:

1. Run `wasp db migrate-dev` to migrate the database to the latest migration
2. Run `wasp start` to start the Wasp application.
3. The application should be running on `localhost:3000`. Open it in your browser, and the welcome page will walk you through your next steps.

### Removing the welcome page

The welcome page in `src/welcome/` walks you through the app on your first run. When you no longer need it:

1. Delete the `src/welcome/` folder.
2. Remove `welcomeSpec` and its import from `main.wasp.ts`.
3. Make the tasks page your home page: in `src/tasks/task.wasp.ts`, change the `TasksRoute` path from `/tasks` to `/`, and update the "My tasks" link in `src/shared/components/Header.tsx` to point to `/`.

## Learn more

To find out more about Wasp, visit out [docs](https://wasp.sh/docs).
