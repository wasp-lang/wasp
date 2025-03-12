---
title: 'Using Product Requirement Documents to Generate Better Web Apps with AI'
authors: [vinny]
image: /img/prd/outsmarting-ai-1600.gif
tags: [ai, prd, product requirement, react, full-stack, generate, hack]
---
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import ImgWithCaption from './components/ImgWithCaption'

# TL;DR

I’m an indiehacker that likes creating lots of fun side-projects, like my SaaS app [CoverLetterGPT](https://CoverLetterGPT.xyz) with ~4,000 users. That’s why I've been on the lookout for AI-assisted coding tools to help me kickstart new full-stack web apps as quickly as possible.

I tried out a bunch, but found that most of them produced codebases that were too simple to work with, or getting a good result was just about as time consuming as coding it myself.

But through the process of trying out different tools and methods, I stumbled across a hack that helped me create comprehensive, functional codebases for full-stack apps with Auth, API routes, Tailwind CSS, DB management, and other more complex features.

**The trick?** Ask [ChatGPT](https://chat.openai.com) to write you a detailed Product Requirement Doc for the app you’d like to create, and then pass this to Wasp’s [GPT Web App Generator](https://magic-app-generator.wasp.sh/).

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/92bf5pjvdpfkdppcer0j.png)

The results are really surprising and give you a far better starter codebase than the other tools I’ve tried (mainly due to the specificity of the generator itself).

And best of all, its free to use! 🤑

# Intro

I’m a self-taught, full-stack web developer and I have a lot of fun building side projects.

For example, the side project I’m most proud of is an open-source cover letter generator SaaS App, [CoverLetterGPT](https://CoverLetterGPT.xyz), which has close to 4,000 users!

I also have a lot of ridiculous side-project ideas, like [this app](https://boozetube.netlify.app/) that can turn your favorite tech influencer’s YouTube videos into a drinking game. 🤣

That’s why I’ve been trying out lots of AI-assisted coding tools to generate **fully-functional, full-stack web apps** as quickly as possible.

There are the obvious tools at the moment, like using ChatGPT and Copilot within your IDE, but new ones are popping up all the time, especially those that act as AI assistants or “agents”.

I’ve gotten a chance to try out some of them, and I even wrote a long-form comparison piece where I put [two such tools to the test](https://dev.to/wasp/smol-ai-vs-wasp-ai-which-is-the-better-ai-junior-developer-4fcb), so check that out if you’re interested.

**But there’s a major problem with these tools**: even though they’re able to generate some good boilerplate code, they often include a lot of errors and don’t make the developer's job *that* much easier in the end.

# Where the problem lies

On paper, AI-assisted coding tools generally save devs time and effort, especially when it comes to isolated code snippets.

On one hand, we have tools like ChatGPT and Copilot, which aid you with refactoring, fixing errors, or generating a snippet of code. It's much like assembling a jigsaw puzzle, where the tools serve you the next piece that fits the immediate gap.

But coding isn't just about filling the next available space; it’s about **envisioning the entire picture**, understanding the broader system and how different pieces interrelate.

![https://media3.giphy.com/media/SrnCKS6s02XT2tw6kz/giphy.gif?cid=7941fdc6b01lfcj3taubztyp823itz03hhy9qx8p0mslbtij&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media3.giphy.com/media/SrnCKS6s02XT2tw6kz/giphy.gif?cid=7941fdc6b01lfcj3taubztyp823itz03hhy9qx8p0mslbtij&ep=v1_gifs_search&rid=giphy.gif&ct=g)

AI-assisted coding tools that behave more like agents have the potential to understand this broader context needed to generate larger codebases, but it’s easier said than done. Currently, most of the tools out there end up generating code that comes full of errors.

Worst of all, some of the code they output can be so messy it actually means *more* work for you.

# How to fix it

AI assistants, much like novice apprentices, need a comprehensive understanding of what they should work towards. To achieve this, you need to craft a detailed outline along with a comprehensive set of instructions to give the AI as much context as possible.

You essentially want to be taking on the role of a Product Manager/Designer and be giving the AI a **Product Requirement Document (PRD)**, i.e. an authoritative document that clearly outlines the

* purpose,
    
* features,
    
* functionality,
    
* and behavior
    

of the product to be developed.

But supplying the PRD is just half the battle. This is because components of your web app within the frontend and backend need to know about each other.

And this is where most of these tools fall short, with tools like [Smol-Developer](https://github.com/smol-ai/developer) creating decent client and server code that work great on their own, but unfortunately don’t work together!

Given this, it seems like an AI tool that already knows the ins and outs of the whole system, that understands the interconnectedness of various parts of a web app, is our best bet.

In short, we need a tool that doesn't just 'do its task' but 'understands the project'.

# The Best Tool for the Job: GPT Web App Generator.

Remember, I’m focusing on generating comprehensive full-stack codebases here, and for that Wasp’s [GPT Web App Generator](https://magic-app-generator.wasp.sh/) gets the job done surprisingly well.

How does it do this? Well, the full answer lies in how [Wasp](https://wasp.sh/) as a framework is able to help you build full-stack React/NodeJS web apps.

It’s beyond the scope of this article to [explain it in full detail](https://wasp.sh/docs), but the TL;DR is that Wasp has a compiler that helps build your app based on a config file. The config file is like a set of **instructions** that its compiler understands and uses to piece together the different parts of the full-stack app for you.

![https://media1.giphy.com/media/heVoZxS2qAGk4Ay5E5/giphy.gif?cid=7941fdc6x2abm5omkgd1d79rz6dt5kzaead3mxu8xt4xuwc2&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media1.giphy.com/media/heVoZxS2qAGk4Ay5E5/giphy.gif?cid=7941fdc6x2abm5omkgd1d79rz6dt5kzaead3mxu8xt4xuwc2&ep=v1_gifs_search&rid=giphy.gif&ct=g)

This is what makes it easier for the AI to get all the pieces of the app right! Once it writes the fundamental client and server code, along with the main config file, the Wasp compiler takes over and pieces it all together, removing a lot of potential possibilities for errors!

In the end, you get a React/NodeJS codebase with features like:

1. full-stack auth
    
2. server config and API routes
    
3. tailwind CSS config and styles
    
4. cron jobs and queues
    
5. email sending
    
6. deployment
    

What’s cool too is that this tool doesn't require you to be highly explicit, [because the specifics are baked into the tool itself](https://dev.to/wasp/how-we-built-a-gpt-code-agent-that-generates-full-stack-web-apps-in-react-nodejs-explained-simply-4f9). In other words, **it saves you tons of time and energy without compromising on the quality or coherence of the end product.**

# The Hack: Getting GPT to write the PRD for you

Ok, but if you’re like me, you don’t really know how to write a good PRD. Plus, writing a detailed PRD can be pretty time-consuming. But luckily ChatGPT knows how.

Thanks, ChatGPT 🙏

So to get really great results out of Wasp’s [GPT Web App Generator](https://magic-app-generator.wasp.sh/), I first ask [ChatGPT](https://chat.openai.com) (using GPT-4) to write a detailed product requirement doc for me, like this:

```plaintext
Write a Product Requirement Document for the following full-stack app:

An app where users can track their house plants and their watering schedule.
```

And then I’ll slightly modify ChatGPT’s output before I pass it to GPT Web App Generator:

```plaintext
Product Requirements Document for a House Plants Tracking Application

1. **Product Title**: GreenLush: Your House Plant Care Companion

2. **Purpose**: 

The GreenLush app is designed to help users manage their house plants and keep track of their watering schedules. This app will serve as a reminder tool, a database for plant types, and a platform for users to know more about house plant care. 

3. **Features and Functionality**:

    3.1. **User Registration & Profile Management**: To allow users to create and manage their account.
    
    3.2. **Plant Database**: A comprehensive directory of house plants, with visuals and information about each type.
    
    3.3. **Plant Profile**: Users can create a profile for each house plant they own, fill in its type, and assign a custom nickname and photo.
    
    3.4. **Watering Schedule**: By selecting or inputting the type of plant, the app will suggest an ideal watering schedule. Users can confirm or customize this schedule and notifications will be sent when it's time to water each specific plant. 

    3.5. **House Plant Care Tips**: A section of the app that provides general care tips and recommendations for house plants.

4. **Behavior of the Product**:

    4.1. Users will be prompted to sign up when they open the app for the first time. 
  
    4.2. Once registered, users will be able to browse the plant database, create and manage plant profiles, set watering schedules, and read plant care tips. 

    4.3. Notification alerts will be sent according to the set watering schedule.
```

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/cdyywqox4zq00lmqjdw2.png)

[GPT Web App Generator](https://magic-app-generator.wasp.sh/) will start generating a plan for your app, execute that plan file by file, and even do some error-checking and fixing.

Pretty neat!

Then, the generated app code can be reviewed before you download it and run it locally. This is nice because sometimes it’s useful to tweak the prompt and a few settings to see if you get better results.

Best of all, the process is free. You don’t even need to use your own API key!

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/z3f440u6xj3oadx21dhw.png)

The picture above is the actual generated, working full-stack app I got out-of-the-box from the example prompt above. All I had to do was initialize the database, register/log in, and BOOM, the app was up and running!

🤩 BTW, If you want to check out the code that GPT Web App Generator created based on the above PRD, go here: [https://magic-app-generator.wasp.sh/result/1f28b518-0cca-4352-84e4-69a4ac04d0fa](https://magic-app-generator.wasp.sh/result/1f28b518-0cca-4352-84e4-69a4ac04d0fa)

There are more examples of types of apps you can build with this tool, [written about here](https://dev.to/wasp/gpt-web-app-generator-let-ai-create-a-full-stack-react-nodejs-codebase-based-on-your-description-2g39), but it’s probably best to just play around with it yourself and see what you can get!

# Conclusion

There are several really cool AI-assisted coding tools out there, but for kickstarting a full-stack React/NodeJS app, I’ve found [GPT Web App Generator](https://magic-app-generator.wasp.sh/) to be the best performing one.

It consistently generates functional, comprehensive full-stack starter codebases that need little to no error-fixing, depending on the complexity of the app.

Couple that with the “PRD hack”, and you can save yourself a substantial amount of time by avoiding writing a ton of boilerplate.
