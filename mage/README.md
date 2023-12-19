# Mage

This directory contains the source code of Mage (aka "GPT Web App Generator" aka "Wasp AI"): a Wasp app (so a full-stack web app) that allows you to create a new Wasp app (inception :)!) from just a short description. It uses ChatGPT in a smart way to accomplish this (so it would be clasified as an AI code agent).

Mage is hosted at https://usemage.ai and you can use it there for free.

You can learn more about it [here](https://wasp-lang.dev/blog/2023/07/10/gpt-web-app-generator) and [here](https://wasp-lang.dev/blog/2023/07/17/how-we-built-gpt-web-app-generator).

## Running locally

Mage is really just a client / UI for calling "Wasp AI", which is AI logic that does all the heavy lifting, and is integrated into Wasp's CLI: `wasp`.
So, if you want to generate Wasp apps via AI locally, on your machine, with your OpenAI keys and your choice of models/parameters, we recommend NOT running the Mage app locally, because it is not so easy, instead we recommend you to do it directly via `wasp` CLI, with `wasp new` or `wasp new:ai` commands. Check our docs on how to install `wasp` CLI: https://wasp-lang.dev/docs/quick-start#installation .

If you still want to run Mage web app locally for some specific reason, most likely because you want to contribute, you will need to do the following:

1. Copy `.env.server.example` into `.env.server` and fill in the missing values. You will basically need to provide Github and Google OAuth creds (and first create OAuth apps on both Github and Google if you don't have them yet - if you are a member of Wasp team ask for dev creds, if not you will have to create your own OAuth apps).
2. Run `wasp db start` and then `wasp start`! It might ask you to do `wasp db migrate-dev`, do that if needed.
3. When running Mage locally, it will be looking for `wasp-cli` binary on your machine to use. To satisfy this requirement, you can go to `waspc/` dir (just next to this one) and run `./run install` there. You will want to check though if that Wasp version matches the version Mage expects (check its Dockerfile to see which version of Wasp it expects).
   If building `waspc/` is too complex for you (you don't have Haskell toolchain set up, taking too long, ...), you can go into the code of Mage, find where it calls `wasp-cli` and modify that temporarily to call `wasp` instead.

## Developing

### Updating Wasp version in Dockerfile

Keep in mind that Mage, when deployed, will install the version of Wasp specified in its Dockerfile.
So, make sure to update that version to be in sync with the version of Wasp that it was developed against.
Most often that should be the current version of Wasp on `main`, even if not released yet.

## Deployment

Mage is currently deployed on Wasp's Fly.io cloud.

Same as the rest of Wasp (blog/docs, CLI, ...), the latest deployed version is tracked on `release` branch.

So if you want to deploy new version of Mage, you should get it in wanted state on `release` branch, and then deploy from there.

Also, before deploying, check that version of `wasp` in `Dockerfile` makes sense.

To deploy it, just run `wasp deploy fly deploy`. You might want to add `--org wasp` if needed.

## FAQ

Q: What is the difference between Wasp AI and Mage? Are those the same thing?
A: When we say "Wasp AI" we refer to logic implemented in `wasp` CLI, while when we say "Mage" we refer to the Mage web app that really serves as a client for "Wasp AI" (calls it in the background). That said, we sometimes use these interchangeably.
