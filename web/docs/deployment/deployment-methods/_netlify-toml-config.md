```toml title="netlify.toml"
[build]
  base = "./.wasp/out/web-app"
  publish = "./build"
  command = "exit 0"

# By default, Netlify only redirects when a path doesn't match an existing file.
# See: https://docs.netlify.com/manage/routing/redirects/rewrites-proxies/#shadowing
[[redirects]]
  from = "/*"
  to = "/200.html"
  status = 200
```
