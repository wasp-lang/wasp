```toml title="netlify.toml"
[build]
  base = "./.wasp/out/web-app"
  publish = "./build"
  command = "exit 0"

[[redirects]]
  from = "/*"
  to = "/200.html"
  status = 200
```
