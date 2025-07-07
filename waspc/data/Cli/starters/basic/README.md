# Basic Starter – A Simple ToDo App

Basic starter is a well-rounded template that showcases the most important bits of working with Wasp.

## Prerequisites

- **Node.js** (newest LTS version recommended): We recommend install Node through a Node version manager, e.g. `nvm`.
- **Wasp** (latest version): Install via
  ```sh
  curl -sSL https://get.wasp.sh/installer.sh | sh
  ```

## Using the template

You can use this template through the Wasp CLI:

```bash
wasp new <project-name>
# or
wasp new <project-name> -t basic
```

## Development

To start the application locally for development or preview purposes:

1. Run `wasp db migrate-dev` to migrate the database to the latest migration
2. Run `wasp start` to start the Wasp application. If running for the first time, this will also install the client and the server dependencies for you.
3. The application should be running on `localhost:3000`. Open in it your browser to access the client.

To improve your Wasp development experience, we recommend installing the [Wasp extension for VSCode](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp).

## Learn more

To find out more about Wasp, visit out [docs](https://wasp.sh/docs).
