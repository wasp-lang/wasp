---
title: Introduction
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import DiscordLink from '../../blog/components/DiscordLink';

:::info
Make sure you've set up Wasp! Check out [Getting Started](/docs/quick-start) first for installation instructions, and then continue with the tutorial. In case of any issues - please, ping us on <DiscordLink />. 
:::

We’ll build a web app to solve every developer's most common problem – finding an excuse to justify our messy work! We will start with a single config file that outlines the full-stack architecture of our app plus several dozen lines of code for our specific business logic. There's no faster way to do it, so we can’t excuse ourselves from building it!

We’ll use Michele Gerarduzzi’s [open-source project](https://github.com/michelegera/devexcuses-api). It provides a simple API and a solid number of predefined excuses. A perfect fit for our needs. Let’s define the requirements for the project: 

- The app must be able to pull excuses data from a public API. 
- Users can save the excuses they like (and your boss doesn't) to the database for future reference.
- Building an app shouldn’t take more than 15 minutes.
- Use modern web dev technologies (NodeJS + React)

As a result – we’ll get a simple and fun pet project. You can find the complete codebase [here](https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/ItWaspsOnMyMachine). 

<img alt="Final result"
     src={useBaseUrl('img/dev-excuses-live-preview.gif')}
/>