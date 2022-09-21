---
title: Wasp - language for developing full-stack Javascript web apps with no boilerplate
authors: [martinsos]
tags: [wasp]
---

import useBaseUrl from '@docusaurus/useBaseUrl';

<!---
Subject idea: Removing boilerplate from web development with a domain specific language.
Subject idea: Creating a domain specific language for developing web apps with less code.
Subject idea: Creating a domain specific language to remove boilerplate from web development.
Subject idea: Using domain specific language to do web development with no boilerplate.
-->

<p align="center">
  <img alt="Wasp logo"
      src={useBaseUrl('img/wasp-logo-wide.png')}
      height="150px"
  />
</p>

For the last year and a half, my twin brother and I have been working on [Wasp](https://wasp-lang.dev): a new programming language for developing full-stack web apps with less code.

Wasp is a **simple declarative language** that makes developing web apps easy while still allowing you to use the latest technologies like **React, Node.js, and Prisma**.

In this post, I will share with you why we believe Wasp could be a big thing for web development, how it works, where we are right now and what is the plan for the future!

<!--truncate-->

## Why Wasp?

You know how to use React, know your way around HTML/CSS/…, know how to write business logic on the backend (e.g. in Node), but when you want to build an actual web app and deploy it for others to use, you drown in all the details and extra work - responsive UI, proper error handling, security, building, deployment, authentication, managing server state on the client, managing database, different environments, ....

<p align="center">
  <img alt="Iceberg of web app development"
      src={useBaseUrl('img/iceberg-of-web-app-dev.png')}
      width="500px"
  />
</p>

Jose Aguinaga described in a fun way the unexpected complexity of web app development in his blog post ["How it feels to learn JavaScript in 2016"](https://hackernoon.com/how-it-feels-to-learn-javascript-in-2016-d3a717dd577f), which still feels relevant 4 years later. 

We are building Wasp because even though we are both experienced developers and have worked on multiple complex web apps in various technologies (JQuery -> Backbone -> Angular -> React, own scripts / makefile -> Grunt -> Gulp -> Webpack, PHP -> Java -> Node.js, …), we still feel **building web apps is harder than it should be**, due to a lot of boilerplate and repetitive work involved in the process.  

The main insight for us was that while the tech stack keeps advancing rapidly, the core requirements of the apps are mostly remaining the same (auth, routing, data model CRUD, ACL, …).

That is why almost 2 years ago we started thinking about **separating web app specification** (what it should do) **from its implementation** (how it should do it).  
This led us to the idea of extracting common web app features and concepts into a special specification language (Wasp), while the implementation details are still described via a modern stack (right now React, Node.js, Prisma).

Our vision with Wasp is to create **a powerful but simple language where you can describe your web app as humanly as possible**.
We want to make the top of that iceberg on the image above as pleasant as possible while making the bottom part much smaller.  
In such language, with just a few words, you can specify pages and their routes, specify which type of authentication you want, define basic entities / data models, describe basic data flow, choose where you want to deploy, implement specific details in React/Node, and let Wasp take care of connecting it all, building it and deploying it.

```css title="Example of wasp code describing part of a simple full-stack web app."
app todoApp {
  title: "ToDo App" /* visible in tab */
}

route "/" -> page Main
page Main {
  component: import Main from "@ext/Main.js"  /* Import your React code. */
}

auth { /* full-stack auth out-of-the-box */
  userEntity: User,
  methods: {
    usernameAndPassword: {}
  }
}

entity User {=psl
  id          Int     @id @default(autoincrement())
  username    String  @unique
  password    String
psl=}
```

Check [here](https://github.com/wasp-lang/wasp/blob/main/examples/tutorials/TodoApp/main.wasp) for the complete example.

## Why a language (DSL), aren’t frameworks solving this already?

Frameworks (like e.g. Ruby on Rails or Meteor) are a big inspiration to us.
However, we want to take things one step further - by designing a language specialized for the domain of web apps (a [DSL](https://en.wikipedia.org/wiki/Domain-specific_language)) we can get rid of a lot of boilerplate and provide a cleaner & simpler developer experience.

On the other hand, we are not trying to replace everything with Wasp nor think that would make sense - just the opposite, Wasp acts as a “glue” between your React and Node.js code, saving you from the grunt work while allowing you to keep the flexibility of writing your own code.
**The majority of the code is still being written in React and Node.js, with Wasp serving as the backbone of your whole application.**

Another benefit of a DSL is that it allows Wasp to **understand the web app’s requirements during the build time and reason about it** before generating the final code, and this is what we are especially excited about.

For example, when generating code to be deployed to production, it could pick the most appropriate architecture based on its understanding of the web app and deploy it to serverless or another type of architecture (or even a combination).
Another example would be reusing your data model logic through all the parts of the stack while defining it just once in Wasp.

DSL opens the potential for optimizations, static analysis, extensibility, and unparalleled ergonomics.

## How does it work?

Wasp compiler compiles the .wasp/React/Node.js source code into just React/Node.js target code.  
Currently, Wasp supports only Javascript, but we plan to add Typescript soon.  
Technical note: Wasp compiler is implemented in Haskell.

![Wasp compilation diagram](/img/wasp-compilation.png)

While right now only React and Node.js are supported, we plan to support multiple other technologies in the future.

Generated code is human readable and can easily be inspected and even ejected if Wasp becomes too limiting.
If not ejecting, there is no need for you to ever look at the generated code - it is generated by Wasp in the background.

Wasp is used via `wasp` CLI - to run wasp project in development, all you need to do is run `wasp start`.

<p align="center">
  <img alt="Wasp CLI output"
      src={useBaseUrl('img/wasp-cli-output.png')}
  />
</p>


## Where is Wasp now and where is it going?

Our big vision is to move as much of the web app domain knowledge as possible into the Wasp language itself, giving Wasp more power and flexibility.

Ultimately, since Wasp would have such a deep understanding of the web app's requirements, we could generate a visual editor on top of it - allowing non-developers to participate in development alongside developers.

Also, Wasp wouldn't be tied to the specific technology but rather support multiple technologies (React/Angular/..., Node/Go/...**.

**Wasp is currently in Alpha** and some features are still rough or missing, there are things we haven’t solved yet and others that will probably change as we progress, but **you can try it out and build and deploy web apps**!

### What Wasp currently supports:
 - ✅ full-stack auth (username & password)
 - ✅ pages & routing
 - ✅ blurs the line between client & server - define your server actions and queries and call them directly in your client code (RPC)!
 - ✅ smart caching of server actions and queries (automatic cache invalidation)
 - ✅ entity (data model) definition with Prisma.io
 - ✅ ACL on frontend
 - ✅ importing NPM dependencies 

### What is coming:
 - ⏳ ACL on backend
 - ⏳ one-click deployment
 - ⏳ more auth methods (Google, Linkedin, ...**
 - ⏳ tighter integration of entities with other features
 - ⏳ themes and layouts
 - ⏳ support for explicitly defined server API
 - ⏳ inline JS - the ability to mix JS code with Wasp code!
 - ⏳ Typescript support
 - ⏳ server-side rendering
 - ⏳ Visual Editor
 - ⏳ support for different languages on the backend
 - ⏳ richer wasp language with better tooling 

**You can check out our repo** at https://github.com/wasp-lang/wasp and **give it a try** at https://wasp-lang.dev/docs -> we are always looking for feedback and suggestions on how to shape Wasp!

We also have a **community** on [Discord](https://discord.com/invite/rzdnErX), where we chat about Wasp-related stuff - join us to see what we are up to, share your opinions or get help with your Wasp project.
