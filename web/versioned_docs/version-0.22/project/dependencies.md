---
title: Dependencies
---

In a Wasp project, dependencies are defined in a standard way for JavaScript projects: using the [package.json](https://docs.npmjs.com/cli/configuring-npm/package-json) file, located at the root of your project. You can list your dependencies under the `dependencies` or `devDependencies` fields.

### Adding a New Dependency

To add a new package, like `date-fns` (a great date handling library), you use `npm`:

```bash
npm install date-fns
```

This command will add the package in the `dependencies` section of your `package.json` file.

You will notice that there are some other packages in the `dependencies` section, like `react` and `wasp`. These are the packages that Wasp uses internally, and you should not modify or remove them.

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

To override a dependency, add the `wasp` field to your `package.json` with an `overriddenDeps` object. The keys are the package names, and the values are **what Wasp currently requires** (not your desired version):

<Tabs>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "dependencies": {
        "react": "19.2.1",
        "react-dom": "19.2.1"
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
      "wasp": {
        "overriddenDeps": {
          "react": "19.2.1",
          "react-dom": "19.2.1"
        }
      }
    }
    ```
  </TabItem>
</Tabs>

In this example:
- You want to use React 18.2.0 (specified in `dependencies`)
- Wasp requires React 19.2.1 (specified in `overriddenDeps`)
- By declaring this, you acknowledge you're deviating from Wasp's tested version

When Wasp updates its requirements in a new release, you'll need to update your `overriddenDeps` values to match. This ensures you consciously acknowledge each change.

:::note

If you need the override to apply to transitive dependencies as well (dependencies of your dependencies), you can use npm's built-in [`overrides`](https://docs.npmjs.com/cli/v11/configuring-npm/package-json#overrides) feature alongside `wasp.overriddenDeps`.

:::
