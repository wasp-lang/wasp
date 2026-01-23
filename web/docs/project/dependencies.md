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

- Testing newer versions before Wasp officially supports them
- Working around bugs in a specific dependency version
- Using older versions for compatibility reasons

:::caution

Overriding dependencies is **unsupported**. Wasp cannot guarantee compatibility when you use different versions than what it was tested with. You assume responsibility for any issues that arise.

:::

To override a dependency, add the `wasp` field to your `package.json` with an `overriddenDeps` object. The keys are the package names, and the values are **what Wasp currently requires** (not your desired version):

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

In this example:
- You want to use React 18.2.0 (specified in `dependencies`)
- Wasp requires React 19.2.1 (specified in `overriddenDeps`)
- By declaring this, you acknowledge you're deviating from Wasp's tested version

When Wasp updates its requirements in a new release, you'll need to update your `overriddenDeps` values to match. This ensures you consciously acknowledge each change.

:::note

If you need the override to apply to transitive dependencies as well (dependencies of your dependencies), you can use npm's built-in [`overrides`](https://docs.npmjs.com/cli/v11/configuring-npm/package-json#overrides) feature alongside `wasp.overriddenDeps`.

:::
