```toml title="netlify.toml"
[build]
  base = "./.wasp/out/web-app"
  publish = "./build"
  command = "exit 0"

[[redirects]]
  from = "/*"
  to = "/index.html"
  status = 200
```
