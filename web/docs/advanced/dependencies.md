---
title: "Dependencies: npm packages, package managers and overrides in Wasp"
sidebar_label: Dependencies
description: Add npm packages to a Wasp project in package.json, use npm, pnpm or Bun, install the packages Wasp features need, override a version Wasp manages, and change the 7-day minimum release age.
---

# Dependencies

In a Wasp project, dependencies are defined in a standard way for JavaScript projects: using the [package.json](https://docs.npmjs.com/cli/configuring-npm/package-json) file, located at the root of your project. You can list your dependencies under the `dependencies` or `devDependencies` fields.

### Adding a New Dependency

To add a new package, like `date-fns` (a great date handling library), you use your package manager:

<PackageManagerCommand npm="npm install date-fns" />

This command will add the package in the `dependencies` section of your `package.json` file.

You will notice that `package.json` already lists some packages, like `@wasp/wasp` and `react`. The `@wasp/wasp` version sets your project's Wasp version, so change it only when you upgrade Wasp. Wasp uses the other packages internally, so don't remove them.

### Package managers {#package-managers}

Wasp works with npm, pnpm and Bun. It doesn't work with Yarn.

When you create a project, <InlineWaspCommand command="wasp new" /> asks which package manager you want. You can also pass it, e.g. `--package-manager pnpm`. Without it, Wasp uses the package manager that ran it, so `pnpm dlx @wasp/cli@latest new my-app` creates a pnpm project.

In an existing project, Wasp goes by the `packageManager` field of your `package.json`, then by the lockfile it finds (`package-lock.json`, `pnpm-lock.yaml` or `bun.lock`), and assumes npm otherwise. It uses that package manager in the commands it tells you to run and in the Dockerfile it generates. Wasp never installs packages itself, so you run the install command after you change `package.json` or pull your teammates' changes.

To move an existing project to another package manager:

1. Delete `node_modules` and the old lockfile.
2. Move the settings of the old package manager. New npm projects come with an `.npmrc` and an `allowScripts` field in `package.json`. In pnpm, both go into `pnpm-workspace.yaml`:

   ```yaml title="pnpm-workspace.yaml"
   minimumReleaseAge: 10080
   minimumReleaseAgeExclude:
     - "@wasp/*"
   allowBuilds:
     "@prisma/client": true
     "@prisma/engines": true
     esbuild: true
     prisma: true
   ```

3. Add `@prisma/client` to your `package.json`, in the version of your `prisma` package, if it isn't there yet.
4. Install the packages with the new package manager, and commit its lockfile.

### Using Packages that are Already Used by Wasp Internally

Wasp internally uses certain dependencies (e.g. React, Prisma, Vite, Express) with specific versions. Your project gets them as peer dependencies of `@wasp/wasp`, so your package manager installs them. Run <InlineWaspCommand command="wasp deps" /> to see them.

To import one of them in your own code, add it to your `package.json` in the version that <InlineWaspCommand command="wasp deps" /> shows. npm also lets you import them without listing them, but pnpm doesn't, and a different version than Wasp's ends up as a second copy of the package.

With npm, peer dependencies have to be installed for this to work, so don't set `legacy-peer-deps` or `omit=peer` in your `.npmrc`.

### Packages that Wasp features need {#feature-packages}

Some Wasp features need extra packages, like `lucia` for auth. Wasp doesn't add them to every app. You install them when you start using the feature:

| Feature | Packages |
| --- | --- |
| [Auth](../features/auth/overview.md) | `lucia` |
| [Social auth](../features/auth/social-auth/overview.md) | `lucia`, `arctic` |
| [Jobs](../features/jobs.md) | `pg-boss` |
| [WebSockets](../features/websockets.md) | `socket.io`, `socket.io-client`, `@socket.io/component-emitter` |
| [SMTP email sender](../features/email.md#smtp) | `nodemailer`, `@types/nodemailer` |
| [Mailgun email sender](../features/email.md#mailgun) | `mailgun.js` |
| [SendGrid email sender](../features/email.md#sendgrid) | `@sendgrid/mail` |
| [Resend email sender](../features/email.md#resend) | `resend` |

Each feature's page has the command that installs its packages. If you forget one, Wasp stops with an error that names the package and the command:

```
Your app uses jobs, so it needs pg-boss. Run `npm install "pg-boss@^8.4.2"`.
```

These packages are optional peer dependencies of `@wasp/wasp`, so your package manager checks their versions against the ones Wasp requires, like it does for React or Prisma.

### Overriding Wasp's Dependencies (Advanced)

If you need to use a different version of a Wasp-managed dependency, you can override it. This is an advanced feature intended for:

- Testing newer versions of dependencies before Wasp officially supports them
- Working around bugs in a specific dependency version
- Using older versions for compatibility reasons

:::caution

This functionality is intended to give you control when absolutely necessary, but it comes with risks.

We recommend that you **don't** use override in production projects. We don't test Wasp with different versions of our dependencies, and we don't guarantee functionality or stability. Incompatibilities might be big and obvious, but they can also be subtle and indirect.

When you override dependencies, it's up to you to test your app thoroughly and validate that it works as expected. If issues arise from using overridden versions, it's also up to you to deal with them.

:::


:::tip

If you find the need to override any dependency, we'd appreciate for you to [post an issue on GitHub](https://github.com/wasp-lang/wasp/issues/new/choose), or a [message on our Discord](https://discord.gg/rzdnErX), explaining your usecase. This will make us aware of your needs, and helps us prioritize giving you a supported solution faster.

:::

To override a dependency that Wasp uses, use the overrides of your package manager. npm and Bun read the [`overrides`](https://docs.npmjs.com/cli/v11/configuring-npm/package-json#overrides) field of `package.json`, and pnpm reads [`overrides`](https://pnpm.io/settings#overrides) in `pnpm-workspace.yaml`. With npm, if you also list the package in `dependencies`, both must use the exact same version string, or npm fails with `EOVERRIDE`:

<Tabs>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "dependencies": {
        "react": "^19.2.1",
        "react-dom": "^19.2.1"
      }
    }
    ```
  </TabItem>

  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "dependencies": {
        "react": "18.2.0",
        "react-dom": "18.2.0"
      },
      "overrides": {
        "react": "18.2.0",
        "react-dom": "18.2.0"
      }
    }
    ```
  </TabItem>
</Tabs>

### Supply Chain Protection

New npm projects include an `.npmrc` file with [`min-release-age`](https://docs.npmjs.com/cli/v11/using-npm/config#min-release-age) set to **7 days** by default, and new pnpm projects have the same setting, [`minimumReleaseAge`](https://pnpm.io/settings#minimumreleaseage), in `pnpm-workspace.yaml`. This prevents the package manager from installing any package version that was published less than 7 days ago, which helps protect against [supply chain attacks](https://en.wikipedia.org/wiki/Supply_chain_attack). Malicious packages are typically detected and removed within hours of publication, so by adding a short delay there's a much smaller chance of being targeted by these attacks.

The same file excludes Wasp's own `@wasp/*` packages, so you can upgrade Wasp on the day a release comes out. Keep that setting when you edit the file.

If you need to install a recently published package, adjust the value in that file. With npm, you can also override it for one command by passing the flag directly:

```sh
npm install some-package --min-release-age=0
```
