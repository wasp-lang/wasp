# Website

This website is built using [Docusaurus 2](https://v2.docusaurus.io/), a modern static website generator.

### Installation

```
$ npm install
```

### Local Development

```
$ npm start
```

This command starts a local development server and open up a browser window. Most changes are reflected live without having to restart the server.

### Build

```
$ npm build
```

This command generates static content into the `build` directory and can be served using any static contents hosting service.

To run this version of code and check that it works correctly, run `npm run serve`.

### Deployment

We are deploying to GitHub pages, and that can easily be done with the following command:

```
$ GIT_USER=<Your GitHub username> USE_SSH=true npm run deploy
```

This command will build the website and push it to the `gh-pages` branch.
