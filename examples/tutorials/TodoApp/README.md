# Todo App Tutorial Example App

This is an example app that complements the [Intro Wasp Tutorial](https://wasp.sh/docs/tutorial/create). It is a simple Todo App that demonstrates how to use Wasp to build a truly full-stack web app quickly and easily.

This example app represents the final state of the tutorial. If you'd prefer to follow it step-by-step, you can find [the tutorial here](https://wasp.sh/docs/tutorial/create).

## Running the App Locally

After you've cloned this repository, you can run the app locally by following these steps:

Install Wasp

```bash
curl -sSL https://get.wasp.sh/installer.sh | sh
```

Position yourself in the project directory, then migrate the DB:

```bash
wasp db migrate-dev
```

Finally, run the app:

```bash
wasp start
```
