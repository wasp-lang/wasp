---
title: "Dependencies: npm packages and overrides in Wasp"
sidebar_label: Dependencies
description: Add npm packages to a Wasp project in package.json, override a version Wasp manages with npm overrides or wasp.overriddenDeps, and change the 7-day min-release-age.
---

# Dependencies

In a Wasp project, dependencies are defined in a standard way for JavaScript projects: using the [package.json](https://docs.npmjs.com/cli/configuring-npm/package-json) file, located at the root of your project. You can list your dependencies under the `dependencies` or `devDependencies` fields.

### Adding a New Dependency

To add a new package, like `date-fns` (a great date handling library), you use `npm`:

<PackageManagerCommand npm="npm install date-fns" />

This command will add the package in the `dependencies` section of your `package.json` file.

You will notice that `package.json` already lists some packages, like `@wasp/wasp` and `react`. The `@wasp/wasp` version sets your project's Wasp version, so change it only when you upgrade Wasp. Wasp uses the other packages internally, so don't modify or remove them.

### Using Packages that are Already Used by Wasp Internally

Wasp internally uses certain dependencies (e.g. React, Prisma, Vite, Express) with specific versions. Your project gets them as peer dependencies of `@wasp/wasp`, so npm installs them and refuses a version that conflicts with the one Wasp requires. Run `wasp deps` to see them.

The generated server also lists a few dependencies of its own, like `socket.io` when you use WebSockets. If you list one of those in your `package.json`, Wasp checks that you use the version it requires.

#### Overriding Wasp's Dependencies (Advanced)

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

To override a dependency that comes with `@wasp/wasp`, use npm's [`overrides`](https://docs.npmjs.com/cli/v11/configuring-npm/package-json#overrides) field. If you also list the package in `dependencies`, both must use the exact same version string, or npm fails with `EOVERRIDE`:

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

To override a dependency that the generated server lists, add the `wasp` field to your `package.json` with an `overriddenDeps` object. The keys are the package names, and the values are **what Wasp currently requires** (not your desired version):

```json title="package.json"
{
  "dependencies": {
    "socket.io": "^4.8.0"
  },
  "wasp": {
    "overriddenDeps": {
      "socket.io": "^4.6.1"
    }
  }
}
```

By declaring this, you acknowledge you're deviating from Wasp's tested version. When Wasp updates its requirements in a new release, you'll need to update your `overriddenDeps` values to match. This ensures you consciously acknowledge each change.

### Supply Chain Protection

New Wasp projects include an `.npmrc` file with [`min-release-age`](https://docs.npmjs.com/cli/v11/using-npm/config#min-release-age) set to **7 days** by default. This prevents npm from installing any package version that was published less than 7 days ago, which helps protect against [supply chain attacks](https://en.wikipedia.org/wiki/Supply_chain_attack). Malicious packages are typically detected and removed within hours of publication, so by adding a short delay there's a much smaller chance of being targeted by these attacks.

The same `.npmrc` excludes Wasp's own `@wasp/*` packages with `min-release-age-exclude[]=@wasp/*`, so you can upgrade Wasp on the day a release comes out. Keep this line when you edit the file.

If you need to install a recently published package, you can temporarily override this by passing the flag directly:

<PackageManagerCommand npm="npm install some-package --min-release-age=0" />

Or you can adjust the value in your project's `.npmrc` file.
