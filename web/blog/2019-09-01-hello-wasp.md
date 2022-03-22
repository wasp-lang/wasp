---
title: Hello Wasp!
authors: [martinsos]
tags: [wasp]
---

About a year or so ago, brother and I started discussing how awesome it would be to have a programming language that would understand what “web app” means. Such language would, on one hand, serve as an expressive specification of the web app, while on the other hand, it would take care of “boring” work for us, while we could focus on the business logic specific for our web app.

Step by step, the idea has started to take a more concrete shape, and Wasp (Web Application SPecification language) came to life! While still very early, we are writing this blog post to explain why are we building Wasp, what is the current status and what the future may hold.

<!--truncate-->

## More specification, less implementation

Imagine you want to create a simple Todo web app.

You would explain it like this to your best buddy web developer: “I want to create a web app with the title ‘Todo App’ that has a single page with a list of tasks. Each task has a description and can be either marked as done or not done. The list starts as empty and tasks can be added, deleted or marked as done. I will send you designs for this. Also, I want a user to be required to register/log in.”

Now, let’s take a look at what needs to be done to implement such an app. We need to choose technologies we are going to use (frontend, backend, database, …), figure out the project file structure, set up the build toolchain, configure linting/auto-formatting/style-guide, set up tests (unit/integration, e2e), set up deployment (production, staging), set up code sharing between frontend and backend, … . Then, once everything is set up, we need to implement basic CRUD functionality (components on frontend and API on the backend), user management, probably some kind of menu on the frontend, …

We can easily see that explanation to web developer (specification) is short and concise because many details are implicit or assumed to be handled in a reasonable default way. On the other hand, implementation is complicated since it has to take care of all the details, many of them not unique for the web app we are building but common for most of the web apps. Also, if we consider the specification through time, it would look the same now and 5 years ago. On the other hand, implementation would be different, due to the new technologies that have emerged in the meantime.

So if the specification is time-resilient, short and relatively simple to describe, while implementation is complex, volatile and requires a lot of expert knowledge, how great would it be to write more of specification and less of implementation when building a web app? For that, we need more powerful languages, that will be able to express more in less code. This is where Wasp comes in.

## Wasp!

The idea behind Wasp is to take everything repetitive and common in the development of a typical web app and have Wasp take care of those parts for us. Ideally, programming in Wasp would very much look like describing the specification to the web developer, therefore writing more specification and less implementation. Wasp is the one who will keep evolving and making sure your specification is implemented in the best possible technology using the industry best practices.

To achieve that, we made Wasp as a DSL (domain-specific language) that understands common concepts of a web app like pages, routes, frontend and backend and their relationship, entities, user and roles/permissions, etc. Other parts, those that are specific for our web app (business logic), we can still write in html/css/js/…, and then plug them into Wasp, combining the power of Wasp with the flexibility of existing technologies.

## What’s up?

We are currently working on the first version of Wasp compiler, and are planning to soon have very first, MVP version ready. It will be just the first step of our vision of what Wasp could be, but the sooner we get it out there, the sooner we can start collecting feedback and further shaping Wasp together with the community.

We believe it will take significant effort to bring Wasp to the level where a big portion of developers will be able to build the whole app with Wasp without feeling restrained by missing flexibility or options, while on the other hand, we don’t want to wait too long until people can start using Wasp. Therefore, we decided to build it from start in such a way that a developer can at any moment “eject” from Wasp and continue on their own, where “ejecting” would mean that Wasp would generate the source code of web app that you can continue working on. That is why compiler for Wasp that we are building is actually a transpiler whose output is web app written with best practices, that you can at any moment take and continue from there if you feel too limited by Wasp. It is like having a senior developer guide you through writing a web app!

This poses the following question: “In which technologies will web app that Wasp transpiler produces be implemented?”. Well, while our vision is to offer multiple flavors here, so that you can choose the combination of technologies that you want to use, for a start we are going with one fixed technology stack, based on most popular technologies: React, Redux, NodeJS, and Mongo.

## Moar

One thing that we are very excited about regarding Wasp is that Wasp understands the way web app is built. So, once you describe it in Wasp, there are many things we could be able to do with it. We could automatically generate tests since we understand the requirements. We could suggest solutions on how to improve the design of the web app. Also, since Wasp should make building web apps easier, we could build solutions on top of it, for example, a visual builder that generates Wasp code, that in turn generates a web app.

We are still very early in the Wasp journey but we are very excited about the opportunities that we imagine it could bring and about the possibilities it could unlock. We hope that this blog post will inspire others to discuss this concept and that together we will create something amazing and learn a lot on the way!
