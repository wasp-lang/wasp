# Mage

This directory contains the source code of Mage (aka "GPT Web App Generator" aka "Wasp AI"): a legacy Wasp app that generates legacy Wasp DSL apps from a short description.

Mage is frozen on Wasp 0.23.0. It is not updated for the new Wasp Spec API.

Mage is hosted at https://usemage.ai and you can use it there for free.

You can learn more about it [here](https://wasp.sh/blog/2023/07/10/gpt-web-app-generator) and [here](https://wasp.sh/blog/2023/07/17/how-we-built-gpt-web-app-generator).

## Running locally

If you want to run Mage locally, you need to use Wasp 0.23.0:

1. Copy `.env.server.example` into `.env.server` and fill in the missing values. You will basically need to provide Github and Google OAuth creds (and first create OAuth apps on both Github and Google if you don't have them yet - if you are a member of Wasp team ask for dev creds, if not you will have to create your own OAuth apps).
2. Install Wasp 0.23.0 with `npm i -g @wasp.sh/wasp-cli@0.23.0`.
3. Run `wasp db start` and then `wasp start`. It might ask you to do `wasp db migrate-dev`, do that if needed.
4. When generating apps locally, Mage looks for the `wasp` binary. You can override this with `MAGE_WASP_CLI_PATH` if needed.

## Deployment

Mage is currently deployed on Wasp's Fly.io cloud.

Same as the rest of Wasp (blog/docs, CLI, ...), the latest deployed version is tracked on `release` branch.

So if you want to deploy new version of Mage, you should get it in wanted state on `release` branch, and then deploy from there.

To deploy it, just run `wasp deploy fly deploy`. You might want to add `--org wasp` if needed.

## FAQ

Q: What is the difference between Wasp AI and Mage? Are those the same thing?<br>
A: When we say "Wasp AI" in Mage, we refer to the legacy AI generator that existed in Wasp 0.23. Mage is the web app that calls that generator in the background.
