---
title: "Dependencies: npm packages and overrides in Wasp"
sidebar_label: Dependencies
description: Add npm packages to a Wasp project in package.json, override a version Wasp manages with wasp.overriddenDeps, and change the 7-day min-release-age.
---

# Dependencies

In a Wasp project, dependencies are defined in a standard way for JavaScript projects: using the [package.json](https://docs.npmjs.com/cli/configuring-npm/package-json) file, located at the root of your project. You can list your dependencies under the `dependencies` or `devDependencies` fields.

### Adding a New Dependency

To add a new package, like `date-fns` (a great date handling library), you use `npm`:

```bash
npm install date-fns
```

This command will add the package in the `dependencies` section of your `package.json` file.

You will notice that there are some other packages in the `dependencies` section, like `react` and `@wasp/wasp`. These are the packages that Wasp uses internally, and you should not modify or remove them.

### Using Packages that are Already Used by Wasp Internally

Wasp internally uses certain dependencies (e.g. React, Prisma, Vite) with specific versions. By default, you cannot specify a different version for these packages - if you try, you'll get an error telling you which version Wasp requires.

#### Overriding Wasp's Dependencies (Advanced)

If you need to use a different version of a Wasp-managed dependency, you can override it using the `wasp.overriddenDeps` field in your `package.json`. This is an advanced feature intended for:

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

The packages Wasp uses are peer dependencies of `@wasp/wasp`, so npm itself refuses to install a conflicting version. To override one, you need two things in your `package.json`: npm's [`overrides`](https://docs.npmjs.com/cli/v11/configuring-npm/package-json#overrides) field, which makes npm accept your version for every package that asks for it, and Wasp's `wasp.overriddenDeps` object, which tells Wasp you know what you are doing. The keys are the package names. In `overriddenDeps`, the values are **what Wasp currently requires** (not your desired version):

<Tabs>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "dependencies": {
        "@wasp/wasp": "0.27.0"
      }
    }
    ```
  </TabItem>

  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "dependencies": {
        "@wasp/wasp": "0.27.0",
        "express": "5.0.1"
      },
      "overrides": {
        "express": "5.0.1"
      },
      "wasp": {
        "overriddenDeps": {
          "express": "~5.1.0"
        }
      }
    }
    ```
  </TabItem>
</Tabs>

In this example:
- You want to use Express 5.0.1 (specified in `dependencies` and forced everywhere through `overrides`)
- Wasp requires Express `~5.1.0` (specified in `overriddenDeps`)
- By declaring this, you acknowledge you're deviating from Wasp's tested version

When Wasp updates its requirements in a new release, you'll need to update your `overriddenDeps` values to match. This ensures you consciously acknowledge each change.

### Supply Chain Protection

New Wasp projects include an `.npmrc` file with [`min-release-age`](https://docs.npmjs.com/cli/v11/using-npm/config#min-release-age) set to **7 days** by default. This prevents npm from installing any package version that was published less than 7 days ago, which helps protect against [supply chain attacks](https://en.wikipedia.org/wiki/Supply_chain_attack). Malicious packages are typically detected and removed within hours of publication, so by adding a short delay there's a much smaller chance of being targeted by these attacks.

If you need to install a recently published package, you can temporarily override this by passing the flag directly:

```bash
npm install some-package --min-release-age=0
```

Or you can adjust the value in your project's `.npmrc` file.
