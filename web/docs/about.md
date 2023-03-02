---
title: Introduction
sidebar_label: What is Wasp?
slug: /about
---

Thanks a lot for giving Wasp a try! In the sections below we will give a short overview of what it is, how it works and get you started.

## What is Wasp?
Wasp is a programming language for building **full-stack web applications**. That means Wasp takes care of all three
major parts of a web application: **client** (front-end), **server** (back-end) and **deployment**.

## Wasp is a DSL
Wasp is a programming language, but a specific kind: It is a *Domain Specific Language*, or shorter *DSL*.
That means it is not a general-purpose, Turing-complete language (such as e.g. Python or Java) and it is not meant
to replace them. Instead, it is specialised for a single purpose: **building modern web applications**.

Other examples of *DSL*s that are often used today are e.g. *SQL* for databases and *HTML* for web page layouts.
The main advantage and reason why *DSL*s exist is that they need to do only one task (e.g. database queries)
so they can do it well and provide the best possible experience for the developer.

The same idea stands behind Wasp - a language that will allow developers to **build modern web applications with
10x less code and less stack-specific knowledge**.

## Wasp integrates with the existing stack
As mentioned above, Wasp is not trying to do everything at once but rather focuses on the accidental complexity
which arises from connecting all the parts of the stack (client, server, deployment) together.

Right now, Wasp supports React and Node and relies on them to define web components and server queries and
actions.

## Is Wasp a web app framework?
Wasp is addressing the same core problems that typical web app frameworks are addressing, and it in big part [looks, swims and quacks](https://en.wikipedia.org/wiki/Duck_test) like a web app framework.

On the other hand, Wasp does not match typical expectations of a web app framework: it is not a set of libraries, it is instead a programming language (DSL).

## What it is meant for
- building full-stack web apps (like e.g. Airbnb or Asana)
- quickly starting a web app with industry best practices
- to be used alongside modern web dev stack (currently supported React and Node)

## What it is not meant for
- building static/presentational websites
- to be used as a no-code solution
- to be a solve-it-all tool in a single language
