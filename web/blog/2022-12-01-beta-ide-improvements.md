---
title: 'Wasp Beta brings major IDE improvements'
authors: [martinsos]
tags: [wasp, language]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'


With the Beta release (0.7), Wasp brings its IDE game to a whole new level!

So far Wasp didn’t have much beyond basic syntax highlighting in VSCode, but now it has:

1. **Wasp language server**, that brings the following to your .wasp files:
    1. live error reporting in your editor
    2. autocompletion (basic for now)
2. **VSCode Wasp language extension**:
    1. snippets (for `page`, `query`, `action`, `entity`)
    2. improved syntax highlighting for .wasp files
    3. integration with the above-mentioned language server
3. Support for popular **IDEs to fully support Javascript and Typescript files** in the Wasp project.

<!--truncate-->

<ImgWithCaption
    alt=""
    source="img/beta-ide-improvements/wls-demo.gif"
    caption="Wasp IDE support in action in VSCode: syntax highlighting, snippets, live error reporting."
/>

Wasp IDE support in action in VSCode: syntax highlighting, snippets, live error reporting.

# Wasp Language Server

Wasp Language Server (WLS) is the “brain” behind smart IDE features like live error reporting and autocompletion - so if it seems like IDE actually understands your code to some degree, well that is the language server!

:::tip
For curious, check out the source code of WLS on Github: [https://github.com/wasp-lang/wasp/tree/main/waspc/waspls/src/Wasp/LSP](https://github.com/wasp-lang/wasp/tree/main/waspc/waspls/src/Wasp/LSP) .
:::

## Features

### Live error/warning reporting

WLS compiles wasp code for you as you work on it and shows you any errors directly in the editor, via red squiggly lines.


<ImgWithCaption
    alt=""
    source="img/beta-ide-improvements/wls-live-errors.gif"
/>

### Autocompletion

WLS understands at which part of code you are right now and offers appropriate completions for it.


<ImgWithCaption
    alt=""
    source="img/beta-ide-improvements/wls-autocompletion.gif"
/>

:::note
Right now WLS is pretty naive here, and mostly focuses on offering available expressions when it realizes you need an expression. This is helpful but just a start, and it will get much smarter in future versions!
:::

## Bit of history: why are Language Servers cool

Years ago, there was no standardized way to write something like Language Server for your language, instead, each language was doing something of its own, and then each editor/IDE would also implement its own layer of logic for using it, and that was a loooot of work that needed to be done for each editor!

Luckily, Microsoft then came up with [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) - a standardized way of communicating between the “smart” part, implemented by language creators, and the editor/IDE part (language extension) that is using it. This enabled each editor to implement this logic for interacting with language servers only once, and then it can be used for any language server!

This is great for us, language creators, because it means that once we implement a language server for our language, most of the work is done, and the work we need to do per each editor is manageable.

Right now WLS is used only by the [VSCode Wasp language extension](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp), but thanks to the nature of the Language Server Protocol, it should be relatively easy to add support for other editors too! Check this [GH issue](https://github.com/wasp-lang/wasp/issues/864) if you are interested in helping.

## Setup

The best thing: there is nothing you, as a Wasp user, have to do to set up WLS! It already comes bundled with your installation of `wasp` → so if you can run `wasp` projects on your machine, you already have WLS, and it is always of the correct version needed for your current wasp installation. The only thing you need to ensure is you have `wasp` version ≥ 0.6, and a relatively fresh VSCode Wasp language extension.

An easy way to check that your version of `wasp` has WLS packaged into it is to run it and look at its usage instructions: it should mention `waspls` as one of the commands.

<ImgWithCaption
    alt=""
    source="img/beta-ide-improvements/wasp-cli-waspls.png"
/>

# Wasp VSCode extension

If we would call Wasp Language Server (WLS) the “backend”, then [VSCode Wasp language extension](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp) would be “frontend” → it takes care of everything to ensure you have a nice experience working with Wasp in VSCode, while delegating the hardest work to the WLS.

<ImgWithCaption
    alt=""
    source="img/beta-ide-improvements/wasp-vscode-extension.png"
/>

:::tip
For curious, you can check out its source code here, core of it is just one file: [https://github.com/wasp-lang/vscode-wasp/blob/main/src/extension.ts](https://github.com/wasp-lang/vscode-wasp/blob/main/src/extension.ts)
:::

## Features

### Syntax highlighting

Nothing unexpected here: it recognizes different parts of Wasp syntax, like type, value, identifier, comment, string, … and colors them appropriately.

If you are curious how is this implemented, check [https://github.com/wasp-lang/vscode-wasp/blob/main/syntaxes/wasp.tmLanguage.yaml](https://github.com/wasp-lang/vscode-wasp/blob/main/syntaxes/wasp.tmLanguage.yaml) → the whole syntax of Wasp is described via this “mysterious” old TextMate format, since that is the way to do it in VSCode.

### Snippets

Wasp allows you to quickly generate a snippet of code for a new `page`, `query`, `action`, or `entity`!

<ImgWithCaption
    alt=""
    source="img/beta-ide-improvements/wls-snippets.gif"
/>

Check out our snippet definitions here: [https://github.com/wasp-lang/vscode-wasp/blob/main/snippets/wasp.json](https://github.com/wasp-lang/vscode-wasp/blob/main/snippets/wasp.json) . It is actually really easy, in VSCode, to define them and add new ones.

### Live error reporting + autocompletion

This is done by delegating the work to WLS, as described above!

# IDE support for Javascript / Typescript in Wasp project

Due to how unique Wasp is in its approach, getting an IDE to provide all the usual features for Javascript / Typescript wasn’t completely working, and instead, the IDE would get somewhat confused with the context in which files are and would for example not be able to offer “go to definition” for some values, or would not know how to follow the import path.

With Wasp Beta this is now resolved! We resolved this by somewhat changing the structure of the Wasp project and also adding tsconfig.json files that provide IDE with the information needed to correctly analyze the JS/TS source files.

To learn more about Typescript support in Wasp Beta, check [this blog post](https://wasp-lang.dev/blog/2022/11/29/typescript-feature-announcement)!

# What does the future hold?

While Wasp Beta greatly improved IDE support for Wasp, there are still quite a few things we want to improve on:

1. Smarter autocompletion via WLS.
    1. Right now it suggests any expression when you need an expression. In the future, we want it to know exactly what is the type of needed expression, and suggest only expressions of that type! So if I am in `route ... { to: <my_cursor_here> }`, then I want to see only `page`s among the suggested completions, not `queries` or `actions` or something else.
    2. Further, we would also like it to autocomplete on dictionary fields → so if I am in `route ... { <my_cursor_here> }`, it should offer me `path` and `to` as completions, as those are only valid fields in the `route` dictionary.
2. Extensions for other editors besides VSCode. Now that we have Wasp Language Server, these shouldn’t be too hard to implement! This is also a great task for potential contributors: check this [GH issue](https://github.com/wasp-lang/wasp/issues/864) if you are interested.
3. Implement Wasp code formatter. We could make it a part of WLS, and then have the editor extension call it on save.
4. Improve support for PSL (Prisma Schema Language) in .wasp files.

If any of these sound interesting, feel free to join us on our [Github](https://github.com/wasp-lang/wasp), or join the discussion on [Discord](https://discord.gg/rzdnErX)!
