---
title: "Built in Days, Acquired for $20K: The NuloApp Story"
authors: [milica]
image: /img/nuloapp/wasp-friends.webp
tags: [webdev, wasp, saas, builders, showcase]
---
import ReactPlayer from 'react-player'
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

Meet [**Kaloyan Stoyanov**](https://www.linkedin.com/in/kaloyan-stoyanov-466a54196/), a tech lead who turned his passion project into a full-fledged SaaS product, and sold it within days of launching it. 

A year before officially launching [NuloApp](https://nuloapp.com/), Kaloyan realized that many creators in the "faceless YouTube channels" niche were using tools like [Opus.pro](http://opus.pro/) to generate short-form content from long-form videos, but these tools were very expensive. Without yet earning revenue from YouTube or TikTok, Kaloyan decided to take matters into his own hands, building his own tool in just a month.

Initially, his tool automatically created and uploaded shorts, but after some time, when his channel didn’t pick up, he stopped the project. Fast forward to a year later, and YouTube's algorithm brought similar content back into his feed, reigniting his passion. This time, Kaloyan took it further by transforming his tool into a SaaS product: NuloApp.

### **The Problem NuloApp Solves**

NuloApp is an AI tool designed to make video content creation simpler by converting long-form videos into short clips that have the highest chance to capture audience’s attention. It resizes content from horizontal (landscape) to vertical (portrait) for platforms like YouTube Shorts, Instagram Reels, and TikTok, helping creators push content faster.

<ReactPlayer playing controls muted={true} url='/img/nuloapp/vid.mp4' />

### **Tech Stack Overview**

- **Framework:** Wasp
- **Payment integration:** Stripe
- **Other tools:** OpenCV, FastAPI, Meta's llama, OpenAI's Whisper, LangChain
- **Database:** PostgreSQL

### **Programatically Editing Videos**

The real genius behind NuloApp is the way that Kaloyan combined a number of tools to programatically edit the longer form videos and podcasts, into short, engaging clips for social media. 

First of all, [OpenCV](https://opencv.org/), an open-source computer vision library, was used as the main editing tool. This is how NuloApp is able to get the correct aspect ratio for smartphone content, and do other cool things like centering the video on the speaker so that they aren't out of frame when the aspect ratio is changed. 

In order to programtically get the correct clips to extract, AI tools like Meta's [llama-3-70b LLM](https://github.com/meta-llama/llama3) and OpenAI's [Whisper](https://github.com/openai/whisper) were also used. Whisper allowed for fast speech-to-text transcription, which could then be passed on the llama in order to find segments worth extracting. 

Putting these tools together and accessible via a standalone API was the final step in this process. But this really clever combination of tools was just one part of puzzle. The next problem to solve was how to deliver it all as a SaaS app that users could pay for?

### **Why Wasp?**

When Kaloyan decided to relaunch his tool as a SaaS product, he didn’t have time to spare. He needed a framework that would allow him to build and deploy quickly. That’s where **Wasp** came in.

“I was looking for a quick and easy-to-use boilerplate with most SaaS app features already pre-built so I could deploy faster,” says Kaloyan. [Wasp’s SaaS boilerplate starter](https://opensaas.sh/), with well-structured documentation, alongside its responsive Discord support, made it the ideal choice.

### **The Impact of Wasp on Development**

Kaloyan was particularly impressed with how Wasp simplified complex tasks that would normally take much longer to implement. From setting up Google logins and dark mode switches to creating hourly jobs, the development process was smoother than expected. “Everything—from enabling Google logins to creating hourly jobs I badly needed—was way too easy to set up.”.

### **Auth and Stripe Integration Made Easy**

One of Kaloyan’s least favorite tasks as a developer is building out authentication systems, and he found that even implementing third-party libraries could be frustrating. Fortunately, Wasp’s boilerplate made the process of setting up authentication and pre-configuring Stripe for payments seamless. 

Here's what `wasp.config` file looks like, through which you can define full-stack auth in a Wasp app.

```jsx
app myApp {
  wasp: {
    version: "^0.14.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the User entity
    userEntity: User,
    methods: {
      // 2. Enable Github Auth
      gitHub: {},
      email: {
        // 3. Specify the email from field
        fromField: {
          name: "My App Postman",
          email: "hello@itsme.com"
        },
        // 4. Specify the email verification and password reset options
        emailVerification: {
          clientRoute: EmailVerificationRoute, //this route/page should be created
        },
        passwordReset: {
          clientRoute: PasswordResetRoute, //this route/page should be created
        },
        // Add an emailSender -- Dummy just logs to console for dev purposes
        // but there are a ton of supported providers :D
        emailSender: {
          provider: Dummy,
        },
      },
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

And here's a 1 minute demo:
<div className='flex justify-center'>
    <iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/Qiro77q-ulI?si=ALZU_PdeKRlq_-Ac" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
</div>

<br/>

Additionally, the framework's job scheduling features helped Kaloyan avoid the headache of configuring cron jobs on Docker containers.

### **Fast Acquisition: From Launch to Sale in 24 Hours**

Upon launching NuloApp, Kaloyan listed the product on [Acquire](http://acquire.com/) with the primary goal of gathering feedback from potential buyers about what features or metrics they value most in a SaaS product. To his surprise, within the first day of listing, he received multiple offers. After a brief meeting with one interested buyer, they quickly agreed on a $20k deal, validating the product's value and market potential.

![NuloApp Homepage](/img/nuloapp/app.png)

### **Advice for Builders Considering Wasp**

If you're considering Wasp, Kaloyan’s advice is clear: Wasp is easy to get started with and flexible enough to build what you need without adding unnecessary complexity. For Kaloyan, Wasp was ideal because it handled the boilerplate while still giving him the freedom to customize as required. "The documentation is also solid, which definitely helps when you're moving quickly.”

### Next Steps

If you’d like to follow in Kaloyan’s footsteps, this is how to get started with the boilerplate he used.

Open your terminal and install Wasp:

``` shell
curl -sSL https://get.wasp-lang.dev/installer.sh | sh
```

From there you only need to run:

``` shell
wasp new -t saas
```

That’s it, you’re one step closer to building your first SaaS!

Feel free to join our [Discord](https://discord.gg/rzdnErX) to connect with other builders and get support from the Wasp community. See you!
