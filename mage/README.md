# Mage

This directory contains the source code of Mage (aka "GPT Web App Generator" aka "Wasp AI"): a Wasp app (so a full-stack web app) that allows you to create a new Wasp app (inception :)!) from just a short description. It uses ChatGPT in a smart way to accomplish this (so it would be classified as an AI code agent).

Mage is hosted at https://usemage.ai and you can use it there for free.

You can learn more about it [here](https://wasp.sh/blog/2023/07/10/gpt-web-app-generator) and [here](https://wasp.sh/blog/2023/07/17/how-we-built-gpt-web-app-generator).

## Running locally

If you want to run Mage locally:

1. Copy `.env.server.example` into `.env.server` and fill in the missing values. You will basically need to provide Github and Google OAuth creds (and first create OAuth apps on both Github and Google if you don't have them yet - if you are a member of Wasp team ask for dev creds, if not you will have to create your own OAuth apps).
2. Install Wasp 0.23 with `npm i -g @wasp.sh/wasp-cli@0.23.0`.
3. Run `wasp db start` and then `wasp start`. It might ask you to do `wasp db migrate-dev`, do that if needed.

## Deployment

Mage is currently deployed on Wasp's Fly.io cloud.

Same as the rest of Wasp (blog/docs, CLI, ...), the latest deployed version is tracked on `release` branch.

So if you want to deploy new version of Mage, you should get it in wanted state on `release` branch, and then deploy from there.

To deploy it, just run `wasp deploy fly deploy`. You might want to add `--org wasp` if needed.

## FAQ

Q: What is the difference between Wasp AI and Mage? Are those the same thing?<br>
A: When we say "Wasp AI" in Mage, we refer to the legacy AI generator that existed in Wasp 0.23. Mage is the web app that calls that generator in the background.
