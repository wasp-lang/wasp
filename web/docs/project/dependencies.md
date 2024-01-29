---
title: Dependencies
---

Specifying npm dependencies in Wasp project is done in a typical way for JS projects: via [package.json](https://docs.npmjs.com/cli/configuring-npm/package-json) file at the top level of the project, via the `dependencies` (or `devDependencies`) field.

:::note Using Packages that are Already Used by Wasp Internally
In the current implementation of Wasp, if Wasp is already internally using a certain npm dependency with a certain version specified, you are not allowed to define that same npm dependency yourself while specifying _a different version_.
If you do that, you will get an error message telling you which exact version you have to use for that dependency.
This means Wasp _dictates exact versions of certain packages_, so for example you can't choose version of React you want to use.

We are currently working on a restructuring that will solve this and some other quirks: check [issue #734](https://github.com/wasp-lang/wasp/issues/734) to follow our progress.
:::
