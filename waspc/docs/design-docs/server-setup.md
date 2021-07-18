# Support for executing custom code on server setup

Right now, only code that dev can write that executes on server are operations (actions and queries) and any code that they import.
They are executed when called by client.

In case that dev wants to write a piece of code that executes on server start, in order to perform some kind of setup, e.g. connect to the database or schedule a cron job, they can't do it.

Additionally, dev currently can't in any way influence or modify default server setup that Wasp performs, which might prove to be too rigid in the future.
While this is a separate concern, it is closely related to the execution of code on server start and is worth considering it at the same time.

NOTE: What dev can currently do, is implement a JS singleton which can then be imported in JS operations and used there.
  Such singleton could be used to allocate resources the very first time it is called, e.g. it could establish connection to the database in a lazy manner.
  This solves the problem above somewhat, in case when it is ok that resource allocation happens lazily on first request.
  It can't be used to customize Wasp setup though, and you can't perform the setup before the very first request, upfront, which means it can't be used to e.g. define a scheduled job.

## Feedback

I found that people are requesting same feature from NextJS, in following discussions:
  - https://github.com/vercel/next.js/discussions/11686
  - https://github.com/vercel/next.js/discussions/15341

They want to run some custom setup, like:
  - Connecting to a database.
  - Starting some jobs.
  - Setting up a store or DB that uses the filesystem. For example, a leveldb instance or a sqlite instance.
  - Reading data from a file.
  - Setting up a local 'scratch' dir, relative to the build file.
  - init DB, configure the logger, even add middlewares.
  - init crons and mongoose.
  - initialize few application/site level parameters on the server which can be reused (at server) on each request and updated on set interval.

NextJS doesn't have a solution for this, instead their official answer is that you should create a standalone microservice for that,
or that you can use a [custom server](https://nextjs.org/docs/advanced-features/custom-server) feature where you use next programmatically in your custom nodejs server, but then you lose a lot of benefits that NextJS provides.

The solution that people suggested was an async function that returns an object (with e.g. allocated resources) that will be included in the "context" that is then passed around to the operations.

## Requirements

### Basic
  - Dev can specify, through wasp language, a JS function that will be executed on server start.
    Such function would be async, take no arguments, and return an object that would be avaialable in operations (through `context`).

### Advanced
  - Instead of returning an object that will be added to `context`, function could return a function that modifies the `context`.
    This gives more control to dev, but it can also lead to them messing up `context`.
  - Function could take arguments which expose certain parts of the server and therefore give the dev an ooportunity to affect certain parts of the app.
    For example, they could modify the expressJS router. Or they would get access to Prisma.
    This again can be problematic as if gives developer space to mess things up.
  - Function could return not just object to be added to `context`, but also other things that modify how Wasp works.
    For example, it could return an expressJS router that will be added to the expressJS router created by Wasp.
    This way, dev can extend different parts of Wasp while not being able to mess up things, since they don't modify existing configuration directly.
    Instead, they return pluggable parts and Wasp plugs them in in the right places.

## Implementation

### Lang design

- New `server` declaration (in .wasp) with `setup` field:  
  ```css
  server: {
    setup: {
      fn: import { myCustomSetup } from '@ext/serverSetup.js'
    }
  }
  ```

- Function could be defined as:  
  ```js
  // In '@ext/serverSetup.js'
  export const myCustomSetup = async () => {
    const someResource = await setupSomeResource()
    return { someResource } 
  }
  ```

- Resources returned during setup could be used from operations as:  
  ```js
  export const myAction = async (args, context) => {
    console.log(context.server.setup.someResource)
  } 
  ```

I considered adding `server` as not a standalone declaration but a field of `app` declaration, but felt that would be too crowded and we already have stuff like `dependencies` and `auth`, so we already decided to go down the route where we don't put everything under the `app` and this way we are consistent with that.

I also considered multiple ideas on how to put the returned object in the `context`, and found that `server.setup` sounds specific enough that it will not clash with anything else nor will need changing in the future.

### MVP

For the very first version, I will go with basic requirements -> no args, and returned object goes under `context.server.setup`.

In the future we can consider implementing some of the advanced requirements.


