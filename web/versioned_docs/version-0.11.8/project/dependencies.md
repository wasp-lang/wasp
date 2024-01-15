---
title: Dependencies
---

Specifying npm dependencies in Wasp project is done via the `dependencies` field in the `app` declaration, in the following way:

```wasp
app MyApp {
  title: "My app",
  // ...
  dependencies: [
    ("redux", "^4.0.5"),
    ("react-redux", "^7.1.3")
  ]
}
```

You will need to re-run `wasp start` after adding a dependency for Wasp to pick it up.

The quickest way to find out the latest version of a package is to run:
```shell
npm view <package-name> version
```

:::note Using Packages that are Already Used by Wasp Internally
In the current implementation of Wasp, if Wasp is already internally using a certain npm dependency with a certain version specified, you are not allowed to define that same npm dependency yourself while specifying _a different version_.
If you do that, you will get an error message telling you which exact version you have to use for that dependency.
This means Wasp _dictates exact versions of certain packages_, so for example you can't choose version of React you want to use.


We are currently working on a restructuring that will solve this and some other quirks that the current dependency system has: check [issue #734](https://github.com/wasp-lang/wasp/issues/734) to follow our progress.
:::
