---
title: Introduction
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import DiscordLink from '../../blog/components/DiscordLink';

:::info
Make sure you've set up Wasp! Check out [Getting Started](/getting-started.md) first for installation instructions, and then continue with the tutorial. In case of any issues - please, ping us on <DiscordLink />. 
:::

We’ll build a web app to solve every developer's most common problem – finding an excuse to justify our messy work! And will do it with a single config file that covers the full-stack app architecture plus several dozen lines of code. In the quickest possible way, so we can’t excuse ourselves from building it!

We’ll use Michele Gerarduzzi’s [open-source project](https://github.com/michelegera/devexcuses-api). It provides a simple API and a solid number of predefined excuses. A perfect fit for our needs. Let’s define the requirements for the project: 

- The app should be able to pull excuses data from a public API. 
- Save the ones you liked (and your boss doesn't) to the database for future reference.
- Building an app shouldn’t take more than 15 minutes.
- Use modern web dev technologies (NodeJS + React)

As a result – we’ll get a simple and fun pet project. You can find the complete codebase [here](https://github.com/wasp-lang/wasp/tree/main/examples/tutorials/ItWaspsOnMyMachine). 

<img alt="Final result"
     src={useBaseUrl('img/dev-excuses-live-preview.gif')}
/>