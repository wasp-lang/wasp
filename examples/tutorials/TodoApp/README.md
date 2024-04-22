# Todo App Tutorial Example App
This is an example app that complements the [Intro Wasp Tutorial](https://wasp-lang.dev/docs/tutorial/create). It is a simple Todo App that demonstrates how to use Wasp to build a truly full-stack web app quickly and easily. 

This example app represents the final state of the tutorial. If you'd prefer to follow it step-by-step, you can find [the tutorial here](https://wasp-lang.dev/docs/tutorial/create).

This project also allows you to run the app in GitHub Codespaces, so you can try out Wasp without installing anything on your machine.

## Running the App in GitHub Codespaces

On the [main Wasp repo page](https://github.com/wasp-lang/wasp), click on the green "Code" button and create a new Codespace.

Give the Codespace some time to install Wasp and finish initializing, then in the terminal run:

```bash
wasp db migrate-dev
```

Once the migration is done, you can start the app by running:

```bash
wasp start
```

This will start the app. The codespace should prompt you with an "Open in Browser" button. If not, click on the "Ports" tab next to the terminal in the bottom of the Codespace and click on the "forwarded address" for port 3000.

## Running the App Locally
After you've cloned this repository, you can run the app locally by following these steps:

Install Wasp

```bash
curl -sSL https://get.wasp-lang.dev/installer.sh | sh
```

Position yourself in the project directory, then migrate the DB:

```bash
wasp db migrate-dev
```

Finally, run the app:

```bash
wasp start
```