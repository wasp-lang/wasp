---
title: Static Public Files
---

If you wish to make static files publicly available at the root of your app, you can do so by placing them in the `public` directory in the `src/client` folder:

```
src
└── client
    ├── public
    │   ├── favicon.ico
    │   └── robots.txt
    └── ...
```

For example, if you have a file `favicon.ico` in the `public` directory, and your app is hosted at `https://myapp.com`, it will be made available at `https://myapp.com/favicon.ico`.

This can be useful if you wish to e.g. override the default `favicon.ico` or provide `robots.txt`.

:::info Usage in client code
You **can't import these files** from your client code, so e.g. `import favicon from './public/favicon.ico'` won't work.
:::
