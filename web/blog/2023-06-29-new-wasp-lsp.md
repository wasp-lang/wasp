---
title: 'Wasp LSP 2.0 - Next-level autocompletion and IDE integration for Wasp projects!'
authors: [matijasos]
image: /img/new-lsp/new-lsp-banner.png
tags: [launch-week, product-update]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    source="img/new-lsp/new-lsp-banner.png"
/>

It's the fourth day of our [Launch Week #3](blog/2023/06/22/wasp-launch-week-three) - today it's all about dev tooling and making sure that the time you spend looking at your IDE is as pleasurable as possible!

**We present the next generation of Wasp LSP (Language Server Protocol) implementation for [VS Code](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp)**! As you might already know, Wasp has its own simple configuration language (`.wasp`) that acts as a glue between your React & Node.js code.

Although it's a very simple, declarative language (you can think of it as a bit nicer/smarter JSON), and having it allows us to completely tailor the developer experience (aka get rid of boilerplate), it also means we have to provide our own tooling for it (syntax highlighting, auto completion, ...).

We started with syntax highlighting, then basic autocompletion and snippet support, but now we really took things to the next level! Writing Wasp code now is much closer to what we had in our mind when envisioning Wasp.

Without further ado, here's what's new:

## ✨ Autocompletion for config object properties (`auth`, `webSocket`, ...)

Until now, Wasp offered autocompletion only for the top-level declarations such as `page` or `app`. Now, it works for any (sub)-property (as one would expect 😅)!

<ImgWithCaption
    source="img/new-lsp/dict-completion.gif"
    caption="Fill out your Wasp configuration faster and with less typos! 💻🚀"
/>

## 🔍 Type Hints

Opening documentation takes you out of your editor and out of your flow. Stay in the zone with in-editor type hints! 💡

<ImgWithCaption
    source="img/new-lsp/type-hints.gif"
/>

## 🚨 Import Diagnostics

Keep tabs on what's left to implement with JS import diagnostics! There's nothing more satisfying than watching those errors vanish. 😌

<ImgWithCaption
    source="img/new-lsp/import-diagnostics.gif"
    caption="Wasp now automatically detects if the function you referenced doesn't exist or is not exported."
/>

## 🔗 Goto Definition

Your Wasp file is the central hub of your project. Easily navigate your code with goto definition and make changes in a snap! 💨

<ImgWithCaption
    source="img/new-lsp/goto-definition.gif"
    caption="Cmd/Ctrl + click and Wasp LSP takes you straight to the function body!"
/>


Don't forget to install [Wasp VS Code extension](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp) and we wish you happy coding! You can get started right away and [try it out here](/docs/quick-start).
