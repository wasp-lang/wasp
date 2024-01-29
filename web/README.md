# Website

This website is built using [Docusaurus 2](https://v2.docusaurus.io/), a modern static website generator.

It consists of three main parts:
 - Landing page ([src/pages/index.js](src/pages/index.js))
 - Blog ([blog/](blog/))
 - Docs ([docs/](docs/))


### Installation

```
$ npm install
```

### Local Development

```
$ npm start
```

This command starts a local development server and opens up a browser window.
Most changes are reflected live without having to restart the server.

### Build

```
$ npm build
```

This command generates static content into the `build` directory and can be served using any static contents hosting service.

To run this version of code and check that it works correctly, run `npm run serve`.

### Deployment

We are deploying to GitHub pages, and that can easily be done with the following command.

First, ensure you are on the `release` branch. Next, run:

```
$ GIT_USER=<Your GitHub username> USE_SSH=true npm run deploy
```

This command will build the website and push it to the `gh-pages` branch,
which will get it deployed to https://wasp-lang.dev !

### Multiple documentation versions

We maintain docs for multiple versions of Wasp.

Docusaurus docs on this: https://docusaurus.io/docs/versioning .

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

This command does everything for us, and since we use Docusaurus' default settings for versions,
there is nothing else we need to do, it will be picked up as the lastest version by default.

#### Which version of docs should I be editing?

If you are writing/updating docs on `main` for the new release of Wasp, you should edit "current" docs (docs/).

If you are (hot)fixing currently published docs on `release`, then you should edit the versioned docs (versioned_docs/{version}), for whatever version you want to do this for. If you want this change to also be present in all the new docs, then you should also do it for the "current" docs (docs/) (yes, that means duplicating the same change).

Prefer doing doc edits on `main`, as that keeps the whole process simpler, and do changes to docs on `release` only if it really matters to fix the already published versioned docs.

#### Deleting versions

We should not keep too many versions of documentation, especially now in Beta when we are moving fast.

Therefore, we should be quite liberal with deleting the older versions of docs.

Also, it might make sense to delete the previous version of docs if only bug fixes were done in the latest version.
