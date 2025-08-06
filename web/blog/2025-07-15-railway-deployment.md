---
title: "Wasp x Railway: deploy your full-stack app with a single CLI command"
authors: [miho]
image: /img/railway-deployment/railway-cover.png
tags: [wasp, deployment, railway]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

Wasp aims to be the complete package for your full-stack development needs, from full-stack type safety to async jobs, we give you all the legos. One thing that every developers needs at some point is deployment. We keep trying out different deployment providers to see which we like the best… and Railway really stood out to us.

## Why Railway?

Railway recently introduced [Railway Metal](https://blog.railway.com/p/launch-week-02-welcome) which is their cool way of saying they have their own independent infrastructure. They’ve been able to optimize their server costs and lower the prices for everybody. This makes Railway not only a powerful and stable platform but also a very reasonably priced option for many projects.

![Railway Dashboard](/img/railway-deployment/railway-dashboard.jpg)

That’s why we are excited to announce that we added support for [Railway](https://railway.com/) one-command deployment to Wasp! (Fun fact: This is provider #2, Wasp CLI already had one-command deployment with [Fly.io](http://fly.io/).)

<!--truncate-->

## How it works

Wasp takes the Railway experience a step further by giving a Wasp-specific deployment command. Instead of manually configuring each service of your application (the database, the server and the client), Wasp orchestrates the entire deployment with a single command:

![wasp deploy railway command in a terminal](/img/railway-deployment/railway-command-terminal.png)

## See it in action

Here's a a 30-second video to showcase how simple it is to deploy a Wasp app to Railway - it takes a single CLI command and ~3 minutes! 

<iframe width="100%" height="500" src="https://www.youtube.com/embed/O1NwlxIKaLw?si=1jGsHHYVtfK-uaff" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen  className='mb-4'></iframe>

All you need to do is run "wasp deploy railway launch". Behind the scenes, the Wasp CLI:

- Deploys a PostgreSQL database
- Sets up your server
- Deploys your client application
- Wires everything together with the environment variables

<ImgWithCaption
    alt="Railway dashboard with Wasp app"
    caption="This is how your Wasp app looks in the Railway dashboard after deploying"
    source="img/railway-deployment/example-app.png"
/>

This is how your Wasp app looks in the Railway dashboard after deploying

After the deployment process finishes you’ll get a URL to your client app which you can share with your grandma. Time to get those cookies as a reward!

<ImgWithCaption
    alt="Old lady giving a thumbs up"
    caption="Your grandma will be proud of you!"
    source="img/railway-deployment/grandma.gif"
/>

## Deploy your app to Railway today

We are shipping this feature with the latest Wasp 0.17.0 version. 

If you want to deploy your app to Railway:

1. Make sure you have the latest version of Wasp installed
2. Register for Railway and [download their CLI](https://docs.railway.com/guides/cli)
3. Navigate to your Wasp project directory
4. Run the command: `wasp deploy railway launch <your-project-name>`

That's it! Your application will be deployed to Railway, complete with database, server, and client services all properly configured and connected.

For more details on deploying your Wasp apps with Railway, check out our [deployment documentation](/docs/deployment/deployment-methods/wasp-deploy/railway).

Use it today and speedrun your app deployment with Wasp. We can’t wait to see what you build next!