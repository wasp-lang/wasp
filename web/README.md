# Website

This website is built using [Docusaurus 3](https://docusaurus.io/), a modern static website generator.

It consists of three main parts:
  - Landing page ([src/pages/index.js](src/pages/index.js))
  - Blog ([blog/](blog/))
  - Docs ([docs/](docs/))


### Installation

```sh
npm install
```

### Local Development

```sh
npm start
```

This command starts a local development server and opens up a browser window.
Most changes are reflected live without having to restart the server.

### Documentation

We have a few guides on how to write documentation:

- The [Writing Guide](https://wasp.sh/docs/writingguide) is our style guide.
- The [Writing Docs](./WRITING-DOCS.md) contains information about the specifics of our Wasp documentation (our structure and helpful plugins).
- The [Docusaurus docs](https://docusaurus.io/docs/markdown-features) contain information about the extra features that Docusaurus provides in MDX files.

### Build

```sh
npm build
```

This command generates static content into the `build` directory and can be served using any static contents hosting service.

To run this version of code and check that it works correctly, run `npm run serve`.

### LLM file snapshots

The build also generates markdown versions of pages and `llms*.txt` files (see [src/plugins/llm-files](src/plugins/llm-files/)). To catch unintended changes in that output, we keep snapshots of a few representative files in [markdown-snapshots/](markdown-snapshots/) and compare them against the build output in CI. The snapshots track the "current" (next, unreleased) docs version, so we have to build the dev version of the docs.

If the check fails and the changes are intended, update the snapshots and commit the result:

``` sh
npm run build-dev
npm run markdown-snapshots:update
```

NOTE: Files in `static/` dir can override whatever is generate on build. Make sure you don't have any stale static files.

### Deployment

We deploy the website to Cloudflare Workers with the [Release - Deploy web](https://github.com/wasp-lang/wasp/actions/workflows/release-web-deploy.yaml) workflow:

1. Make sure you have the `release` branch ready with all the changes you want to deploy.
2. Run the workflow from the GitHub UI and select the `release` branch.

The workflow builds the website, deploys it to https://wasp.sh, and restarts the Algolia docs search crawler.

The Worker serves the static build and handles markdown content negotiation (`worker.ts`). Its configuration is in `wrangler.jsonc`.

### Preview docs from the `main` branch

The [CI - Deploy web preview](https://github.com/wasp-lang/wasp/actions/workflows/ci-web-deploy-preview.yaml) workflow deploys the docs, including the "current" version, to the `wasp-docs-on-main` Worker on every push to `main`. PRs from this repository get their own Worker Preview, linked in a PR comment.

### Multiple documentation versions

We maintain docs for multiple versions of Wasp.

Docusaurus docs on this: https://docusaurus.io/docs/versioning.

Docusaurus recognizes "current" docs, which are docs in ./docs dir, and also
individual versioned docs, which are docs under versioned_docs/{version}/.
So we have 1 "current" docs and a number of versioned docs.

We stick with Docusaurus' recommended/default approach for multiple doc versions, which says that "current" docs, which reside in `docs/` dir and are served under `/docs/next` URL, are work in progress (even though they are named "current", which is a bit misleading).
So "current" docs are docs for the version of Wasp that is currently in development, not for the last released version, and they are not meant to be consumed by typical Wasp user.

Each versioned documentation consists of versioned_docs/{version} and
versioned_sidebars/{version}.
There is also versions.json file which is just a list of versioned docs.

By default, "current" docs are served under URL {baseUrl}/docs/next,
each versioned doc is served under URL {baseUrl}/docs/{version},
and last versioned docs (first version in versions.json)
are served under URL {baseUrl}/docs/, as the default/latest docs.

Since we don't want our users to read `docs/next` ("current" docs), we don't publish these when we deploy docs, instead we build them only during development.

#### When/how do we create new version of docs from "current" docs?

When releasing new version of Wasp, what we do is run `npm run docusaurus docs:version {version}` to create new versioned docs from the current docs. We do this on every new Wasp release.

The version should not be a full version (e.g. `0.21.0`), but valid for all the non-breaking changes in that range (e.g. `0.21`).

Note: This means users can't access docs for a specific patch version (e.g., `0.21.2`) - they always see the docs for the latest patch in that minor version (e.g., `0.21.4`). This could lead to users reading about features not yet available in their exact version. We consider this an acceptable trade-off for now, since upgrading within the same minor version has no breaking changes. We may revisit this approach in the future if needed.

This command does everything for us, and since we use Docusaurus' default settings for versions,
there is nothing else we need to do, it will be picked up as the latest version by default.

#### Which version of docs should I be editing?

If you are writing/updating docs on `main` for the new release of Wasp, you should edit "current" docs (docs/).

If you are (hot)fixing currently published docs on `release`, then you should edit the current docs (docs/), and then regenerate the versioned docs for the current version (versioned_docs/{version}), for whatever version you want to do this for. You can do that by running `npm run remove-version {version}` to remove the old versioned docs, and then `npm run docusaurus docs:version {version}` to create new versioned docs from the current docs.

Prefer doing doc edits on `main`, as that keeps the whole process simpler, and do changes to docs on `release` only if it really matters to fix the already published versioned docs.

#### Deleting versions

We should not keep too many versions of documentation, especially now in Beta when we are moving fast.

Therefore, we should be quite liberal with deleting the older versions of docs.

Also, it might make sense to delete the previous version of docs if only bug fixes were done in the latest version.

#### Latest Wasp version variable

We have a custom remark plugin that allows us to use `{latestWaspVersion}` in our code blocks, which will be replaced with the latest Wasp version in `versions.json` when the docs are built. This allows us to have the up to date Wasp version in the docs, without having to manually update it every time we release a new version of Wasp.

Check the plugin [here](./src/remark//search-and-replace.js) for more info.
