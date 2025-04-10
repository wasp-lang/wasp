---
title: Migration from 0.16.X to 0.17.X
---

## What's new in 0.17.0?

### Wasp will no longer generate `favicon.ico` on compile

Wasp now provides a default `favicon.ico` in the project's `public` folder.
Deleting the `favicon.ico` results in no favicon in the built application. 

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Add a default `favicon.ico` in the public folder

This step is necessary only if you have no `favicon.ico` in your `public` folder.
If so, add a default `favicon.ico` to your `public` folder.

### 2. Enjoy your updated Wasp app

That's it!

You should now be able to run your app with the new Wasp 0.17.0.
