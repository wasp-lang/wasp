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

::::info
In the current implementation of Wasp, if Wasp is already internally using a certain npm dependency with a certain version specified, you are not allowed to define that same npm dependency yourself while specifying a different version.
If you do that, you will get an error message telling you which exact version you have to use for that dependency.
This means Wasp dictates exact versions of certain packages, so for example you can't choose version of React you want to use.
We are currently working on a restructuring that will solve this and some other quirks that the current dependency system has: check [issue #734](https://github.com/wasp-lang/wasp/issues/734) to follow our progress!
