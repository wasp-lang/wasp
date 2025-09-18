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

In the current version of Wasp, if Wasp is already internally using a certain dependency (e.g. React) with a certain version specified, you are not allowed to define that same npm dependency yourself while specifying _a different version_.

If you do that, you will get an error message telling you which exact version you have to use for that dependency.
This means Wasp _dictates exact versions of certain packages_, so for example you can't choose the version of React you want to use.

:::note

We are currently working on a restructuring that will solve this and some other quirks: check [issue #1644](https://github.com/wasp-lang/wasp/issues/1644) to follow our progress.

:::
