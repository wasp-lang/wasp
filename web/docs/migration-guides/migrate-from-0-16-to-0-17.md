## title: Migration from 0.16.X to 0.17.X

## What's new in 0.17.0?

### Wasp no longer generates a default `favicon.ico` 

Wasp will no longer generate `favicon.ico` if there isn't one in the `public` directory.
Also, Wasp will no longer generate a `<link>` meta tag in `index.html`. You'll need to define it yourself explicitly. 

New Wasp projects come with a default `favicon.ico` in the `public` directory and the `<link>` meta tag in the `main.wasp` file.

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Add a `favicon.ico` to the `public` directory

This step is necessary only if you don't have a `favicon.ico` in your `public` folder.
If so, you should add a `favicon.ico` to your `public` folder. 

If you want to keep the default, you can [download it here](https://github.com/wasp-lang/wasp/tree/main/waspc/data/Cli/templates/skeleton/public/favicon.ico).

### 2. Add a `<link>` meta tag for `favicon.ico`

This step is required for all of the project's which use `favicon.ico`.
Add the `<link>` meta tag to the `head` property in the `main.wasp`

```wasp title="main.wasp
app MyApp {
  // ...
  head: [
    "<link rel='icon' href='/favicon.ico' />",
  ]
}
```

### 3. Enjoy your updated Wasp app

That's it!

You should now be able to run your app with the new Wasp 0.17.0.
