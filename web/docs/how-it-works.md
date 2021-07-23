---
title: How it works
---

At its core, Wasp works like any other language: input files written in the source, higher-level language are
fed into the compiler which then produces the code in a target, lower-level language. 

This is how it looks in the case of Wasp:
- **source**: `.wasp` files along with NodeJS functions and React components.
- **compiler**: `waspc`, a program written in Haskell
- **target**: a web application written in React and NodeJS

Here is also a high-level diagram illustrating the described process:

![Wasp compilation diagram](/img/wasp-compilation.png)
