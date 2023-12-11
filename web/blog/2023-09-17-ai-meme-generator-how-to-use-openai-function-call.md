---
title: "Build your own AI Meme Generator & learn how to use OpenAI's function calls"
authors: [vinny]
image: /img/memerator-banner-tall.gif
tags: [ai, meme, openai, function calling, react, full-stack, generate]
---

## Table of Contents

- [TL;DR](#tldr)
- [Intro](#intro)
    - [Call Me, Maybe](#intro)
    - [Let's Build](#lets-build)
- [Part 1](#config)
    - [Configuration](#config)
        - [Project Set Up](#setup)
        - [Database Set up](#db)
        - [Environment Variables](#env)
        - [Start Your App](#start)
    - [Generating Memes](#generate-meme)
        - [Server-Side Code](#generate-server)
        - [Client-Side Code](#generate-client)
- [Part 2](#part-two)
    - [Fetching & Updating Templates w/ Cron Jobs](#templates)
        - [Defining our Daily Cron Job](#cron)
        - [Testing](#cron-test)
    - [Editing Memes](#edit)
        - [Server-Side Code](#server)
        - [Client-Side Code](#client)
    - [Deleting Memes](#delete)
- [Conclusion](#conclusion)

<a name='tldr' href='#tldr'></a>
# TL;DR

In this two-part tutorial, we’re going to build a full-stack instant Meme Generator app using:

- React & NodeJS w/ TypeScript
- OpenAI’s [Function Calling API](https://platform.openai.com/docs/guides/gpt/function-calling)
- ImgFlip.com’s [meme creator API](https://imgflip.com/api)

<iframe width="560" height="315" src="https://www.youtube.com/embed/4rTXljsphQ8?si=Lny4ruPRNJu3-zwL" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

You check out a deployed version of the app we’re going to build here: [The Memerator](https://damemerator.netlify.app)

If you just want to see the code for the finished app, check out the [Memerator’s GitHub Repo](https://github.com/vincanger/memerator)

<a name='intro' href='#intro'></a>
# Intro

## Call Me, Maybe

With [OpenAI’s chat completions API](https://platform.openai.com/docs/guides/gpt), developers are now able to do some really cool stuff. It basically enables ChatGPT functionality, but in the form of a callable API you can integrate into any app.

But when working with the API, a lot of devs wanted GPT to give back data in a format, like JSON, that they could use in their app’s functions.

Unfortunately, if you asked ChatGPT to return the data in a certain format, it wouldn’t always get it right. Which is why [OpenAI released function calling](https://platform.openai.com/docs/guides/gpt/function-calling).

As they describe it, function calling allows devs to “… describe functions to GPT, and have the model intelligently choose to output a JSON object containing arguments to call those functions.”

This is a great way to turn natural language into an API call.

So what better way to learn how to use GPT’s function calling feature than to use it to call [Imgflip.com’s meme creator API](https://imgflip.com/api)!?

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/0a0bc9uxabyg8cue2axu.png)

<a name='lets-build' href='#lets-build'></a>
## Let’s Build

In this two-part tutorial, we’re going to build a full-stack React/NodeJS app with:

- Authentication
- Meme generation via OpenAI’s function calling and ImgFlip.com’s API
- Daily cron job to fetch new meme templates
- Meme editing and deleting
- and more!

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/dwkbfq9kx8xlu7fb25v0.png)

I already deployed a working version of this app that you can try out here: [https://damemerator.netlify.app](https://damemerator.netlify.app) — so give it a go and let’s get… going.

In Part 1 of this tutorial, we will get the app set up and generating and displaying memes. 

In Part 2, we will add more functionality, like recurring cron jobs to fetch more meme templates, and the ability to edit and delete memes.

BTW, two quick tips:

1. if you need to reference the app’s finished code at any time to help you with this tutorial, you can check out the app’s [GitHub Repo here](https://github.com/vincanger/memerator). 
2. if you have any questions, feel free to hop into the [Wasp Discord Server](https://discord.gg/rzdnErX) and ask us!

# Part 1

<a name='config' href='#config'></a>

## Configuration

We’re going to make this a full-stack React/NodeJS web app so we need to get that set up first. But don’t worry, it won’t take long AT ALL, because we will be using [Wasp](https://wasp-lang.dev) as the framework.

Wasp does all the heavy lifting for us. You’ll see what I mean in a second.

<a name='setup' href='#setup'></a>

### Set up your Wasp project

First, install Wasp by running this in your terminal:

```bash
curl -sSL <https://get.wasp-lang.dev/installer.sh> | sh
```

Next, let’s clone the `start` branch of the [Memerator](http://damemerator.netlify.app) app that I’ve prepared for you:

```bash
git clone -b start https://github.com/vincanger/memerator.git
```

Then navigate into the `Memerator` directory and open up the project in VS Code:

```bash
cd Memerator && code .
```

You’ll notice Wasp sets up your full-stack app with a file structure like so:

```bash
.
├── main.wasp             # The wasp config file.
└── src
    ├── client            # Your React client code (JS/CSS/HTML) goes here.
    ├── server            # Your server code (Node JS) goes here.
    └── shared            # Your shared (runtime independent) code goes here.

```

Let’s check out the `main.wasp` file first. You can think of it as the “skeleton”, or instructions, of your app. This file configures most of your full-stack app for you 🤯:

```jsx
app Memerator {
  wasp: {
    version: "^0.11.3"
  },
  title: "Memerator",
  client: {
    rootComponent: import { Layout } from "@client/Layout",
  },
  db: {
    system: PostgreSQL,
    prisma: {
      clientPreviewFeatures: ["extendedWhereUnique"]
    }
  },
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {}
    },
    onAuthFailedRedirectTo: "/login",
    onAuthSucceededRedirectTo: "/"
  },
  dependencies: [
    ("openai", "4.2.0"),
    ("axios", "^1.4.0"),
    ("react-icons", "4.10.1"),
  ]
}

entity User {=psl
    id       Int    @id @default(autoincrement())
    username String @unique
    password String
    memes    Meme[]
    isAdmin  Boolean @default(false)
    credits  Int     @default(2)
psl=}

entity Meme {=psl
    id         String @id @default(uuid())
    url        String 
    text0      String
    text1      String
    topics     String 
    audience   String 
    template   Template @relation(fields: [templateId], references: [id])
    templateId String
    user       User   @relation(fields: [userId], references: [id])
    userId     Int    
    createdAt  DateTime @default(now())
psl=}

entity Template {=psl
    id       String @id @unique
    name     String
    url      String
    width    Int
    height   Int
    boxCount Int
    memes    Meme[]
psl=}

route HomePageRoute { path: "/", to: HomePage }
page HomePage {
  component: import { HomePage } from "@client/pages/Home",
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import Login from "@client/pages/auth/Login"
}
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import Signup from "@client/pages/auth/Signup"
}
```

As you can see, our `main.wasp` config file has our:

- dependencies,
- authentication method,
- database type, and
- database models (”entities”)
- client-side pages & routes

You might have also noticed this `{=psl psl=}` syntax in the entities above. This denotes that anything in between these `psl` brackets is actually a different language, in this case, [Prisma Schema Language](https://www.prisma.io/docs/concepts/components/prisma-schema). Wasp uses Prisma under the hood, so if you've used Prisma before, it should be straightforward.

Also, make sure you install the [Wasp VS code extension](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp) so that you get nice syntax highlighting and the best overall dev experience.

<a name='db' href='#db'></a>

### Setting up the Database

We still need to get a Postgres database setup. 

Usually this can be pretty annoying, but with Wasp it’s really easy.

1. just have [Docker Deskop](https://www.docker.com/products/docker-desktop/) installed and running, 
2. open up **a separate terminal tab/window,** 
3. `cd` into the `Memerator` directory, and then run

```bash
wasp start db
```

This will start and connect your app to a Postgres database for you. No need to do anything else! 🤯 

Just leave this terminal tab, along with docker desktop, open and running in the background.

Now, in a different terminal tab, run

```bash
wasp db migrate-dev
```

and make sure to give your database migration a name, like `init`.

<a name='env' href='#env'></a>

### Environment Variables

In the root of your project, you’ll find a `.env.server.example` file that looks like this:

```bash
# set up your own credentials on https://imgflip.com/signup and rename this file to .env.server
# NOTE: make sure you register with Username and Password (not google) 
IMGFLIP_USERNAME=
IMGFLIP_PASSWORD=

# get your api key from https://platform.openai.com/
OPENAI_API_KEY=

JWT_SECRET=asecretphraseatleastthirtytwocharacterslong
```

Rename this file to `.env.server` and follow the instructions in it to get your:

- [Imgflip credentials](https://imgflip.com/signup) and
- [OpenAI API keys](https://platform.openai.com/)

as we will need them to generate our memes 🤡

<a name='start' href='#start'></a>

### Start your App

With everything setup correctly, you should now be able to run 

```bash
wasp start
```

When running `wasp start`, Wasp will install all the necessary npm packages, start our NodeJS server on port 3001, and our React client on port 3000. 

Head to [localhost:3000](http://localhost:3000/) in your browser to check it out. We should have the basis for our app that looks like this:

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/l1541hswduhm7d2bm8oz.png)

<a name='generate-meme' href='#generate-meme'></a>

## Generating a Meme

The boilerplate code already has the client-side form set up for generating memes based on:

- topics
- intended audience

This is the info we will send to the backend to call the OpenAI API using function calls. We then send this info to the [imglfip.com API](https://imgflip.com/api) to generate the meme.

But the **/caption_image** endpoint of the imgflip API needs the meme template id. And to get that ID we first need to fetch the available meme templates from imgflip’s **/get_memes** endpoint

So let’s set that up now.

<a name='generate-server' href='#generate-server'></a>

### Server-Side Code

Create a new file in `src/server/` called `utils.ts`:

```tsx
import axios from 'axios';
import { stringify } from 'querystring';
import HttpError from '@wasp/core/HttpError.js';

type GenerateMemeArgs = {
  text0: string;
  text1: string;
  templateId: string;
};

export const fetchMemeTemplates = async () => {
  try {
    const response = await axios.get('https://api.imgflip.com/get_memes');
    return response.data.data.memes;
  } catch (error) {
    console.error(error);
    throw new HttpError(500, 'Error fetching meme templates');
  }
};

export const generateMemeImage = async (args: GenerateMemeArgs) => {
  console.log('args: ', args);

  try {
    const data = stringify({
      template_id: args.templateId,
      username: process.env.IMGFLIP_USERNAME,
      password: process.env.IMGFLIP_PASSWORD,
      text0: args.text0,
      text1: args.text1,
    });

    // Implement the generation of meme using the Imgflip API
    const res = await axios.post('https://api.imgflip.com/caption_image', data, {
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
    });

    const url = res.data.data.url;

    console.log('generated meme url: ', url);

    return url as string;
  } catch (error) {
    console.error(error);
    throw new HttpError(500, 'Error generating meme image');
  }
};
```

This gives us some utility functions to help us fetch all the meme templates that we can possibly generate meme images with.

Notice that the POST request to the **/caption_image** endpoint takes the following data:

- our imgflip **username** and **password**
- **ID** of the meme template we will use
- the text for top of the meme, i.e. **text0**
- the text for the bottom of the meme, i.e. **text1**

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/gt16im7qb0agg6m2idft.png)

The text0 and text1 arguments will generated for us by our lovely friend, ChatGPT. But in order for GPT to do that, we have to set up its API call, too.

To do that, create a new file in `src/server/` called `actions.ts`. 

Then go back to your `main.wasp` config file and add the following Wasp Action at the bottom of the file:

```tsx
//...

action createMeme {
  fn: import { createMeme } from "@server/actions.js",
  entities: [Meme, Template, User]
}
```

An [Action is a type of Wasp Operation](https://wasp-lang.dev/docs/data-model/operations/actions) that changes some state on the backend. It’s essentially a NodeJS function that gets called on the server, but Wasp takes care of setting it all up for you.

This means you don't have to worry about building an HTTP API for the Action, managing server-side request handling, or even dealing with client-side response handling and caching. Instead, you just write the business logic!

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/i1qoo0k5hg2hl3i49lqb.png)

If you’ve got the [Wasp VS Code extension installed](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp), you’ll see an error (above). Hover over it and click `Quick Fix` > `create function createMeme`.

This will scaffold a `createMeme` function (below) for you in your `actions.ts` file if the file exists. Pretty Cool!

```tsx
import { CreateMeme } from '@wasp/actions/types'

type CreateMemeInput = void
type CreateMemeOutput = void

export const createMeme: CreateMeme<CreateMemeInput, CreateMemeOutput> = async (args, context) => {
  // Implementation goes here
}
```

You can see that it imports the Action types for you as well.

Because we will be sending the `topics` array and the intended `audience` string for the meme from our front-end form, and in the end we will return the newly created `Meme` entity, that’s what we should define our types as. 

Remember, the `Meme` entity is the database model we defined in our `main.wasp` config file.

Knowing that, we can change the content of `actions.ts` to this:

```tsx
import type { CreateMeme } from '@wasp/actions/types'
import type { Meme } from '@wasp/entities';

type CreateMemeArgs = { topics: string[]; audience: string };

export const createMeme: CreateMeme<CreateMemeArgs, Meme> = async ({ topics, audience }, context) => {
  // Implementation goes here
}
```

Before we implement the rest of the logic, let’s run through how our `createMeme` function should work and how our `Meme` will get generated:

1. fetch the imgflip meme template we want to use
2. send its name, the topics, and intended audience to OpenAI’s chat completions API
3. tell OpenAI we want the result back as arguments we can pass to our next function in JSON format, i.e. OpenAI’s [function calling](https://platform.openai.com/docs/guides/gpt/function-calling)
4. pass those arguments to the imgflip **/caption-image** endpoint and get our created meme’s url
5. save the meme url and other info into our DB as a `Meme` entity

With all that in mind, go ahead and entirely replace the content in our `actions.ts` with the completed `createMeme` action:

```tsx
import HttpError from '@wasp/core/HttpError.js';
import OpenAI from 'openai';
import { fetchMemeTemplates, generateMemeImage } from './utils.js';

import type { CreateMeme } from '@wasp/actions/types';
import type { Meme, Template } from '@wasp/entities';

type CreateMemeArgs = { topics: string[]; audience: string };

const openai = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

export const createMeme: CreateMeme<CreateMemeArgs, Meme> = async ({ topics, audience }, context) => {
  if (!context.user) {
    throw new HttpError(401, 'You must be logged in');
  }

  if (context.user.credits === 0 && !context.user.isAdmin) {
    throw new HttpError(403, 'You have no credits left');
  }

  const topicsStr = topics.join(', ');

  let templates: Template[] = await context.entities.Template.findMany({});

	if (templates.length === 0) {
    const memeTemplates = await fetchMemeTemplates();
    templates = await Promise.all(
      memeTemplates.map(async (template: any) => {
        const addedTemplate = await context.entities.Template.upsert({
          where: { id: template.id },
          create: {
            id: template.id,
            name: template.name,
            url: template.url,
            width: template.width,
            height: template.height,
            boxCount: template.box_count
          },
          update: {}
        });

        return addedTemplate;
      })
    );
  }

  // filter out templates with box_count > 2
  templates = templates.filter((template) => template.boxCount <= 2);
  const randomTemplate = templates[Math.floor(Math.random() * templates.length)];

  console.log('random template: ', randomTemplate);

  const sysPrompt = `You are a meme idea generator. You will use the imgflip api to generate a meme based on an idea you suggest. Given a random template name and topics, generate a meme idea for the intended audience. Only use the template provided`;
  const userPrompt = `Topics: ${topicsStr} \n Intended Audience: ${audience} \n Template: ${randomTemplate.name} \n`;

  let openAIResponse: OpenAI.Chat.Completions.ChatCompletion;
  try {
    openAIResponse = await openai.chat.completions.create({
      messages: [
        { role: 'system', content: sysPrompt },
        { role: 'user', content: userPrompt },
      ],
      functions: [
        {
          name: 'generateMemeImage',
          description: 'Generate meme via the imgflip API based on the given idea',
          parameters: {
            type: 'object',
            properties: {
              text0: { type: 'string', description: 'The text for the top caption of the meme' },
              text1: { type: 'string', description: 'The text for the bottom caption of the meme' },
            },
            required: ['templateName', 'text0', 'text1'],
          },
        },
      ],
      function_call: {
        name: 'generateMemeImage',
      },
      model: 'gpt-4-0613',
    });
  } catch (error: any) {
    console.error('Error calling openAI: ', error);
    throw new HttpError(500, 'Error calling openAI');
  }

  console.log(openAIResponse.choices[0]);

  /**
   * the Function call returned by openAI looks like this:
   */
  // {
  //   index: 0,
  //   message: {
  //     role: 'assistant',
  //     content: null,
  //     function_call: {
  //       name: 'generateMeme',
  //       arguments: '{\n' +
  //         `  "text0": "CSS you've been writing all day",\n` +
  //         '  "text1": "This looks horrible"\n' +
  //         '}'
  //     }
  //   },
  //   finish_reason: 'stop'
  // }
  if (!openAIResponse.choices[0].message.function_call) throw new HttpError(500, 'No function call in openAI response');

  const gptArgs = JSON.parse(openAIResponse.choices[0].message.function_call.arguments);
  console.log('gptArgs: ', gptArgs);

  const memeIdeaText0 = gptArgs.text0;
  const memeIdeaText1 = gptArgs.text1;

  console.log('meme Idea args: ', memeIdeaText0, memeIdeaText1);

  const memeUrl = await generateMemeImage({
    templateId: randomTemplate.id,
    text0: memeIdeaText0,
    text1: memeIdeaText1,
  });

  const newMeme = await context.entities.Meme.create({
    data: {
      text0: memeIdeaText0,
      text1: memeIdeaText1,
      topics: topicsStr,
      audience: audience,
      url: memeUrl,
      template: { connect: { id: randomTemplate.id } },
      user: { connect: { id: context.user.id } },
    },
  });

  return newMeme;
};
```

At this point, the code above should be pretty self-explanatory, but I want to highlight a couple points:

1. the `context` object is passed through to all Actions and Queries by Wasp. It contains the [Prisma](https://prisma.io) client with access to the DB entities you defined in your  `main.wasp` config file.
2. We first look for the imgflip meme templates in our DB. If none are found, we fetch them using our `fetchTemplates` utility function we defined earlier. Then we `upsert` them into our DB.
3. There are some meme templates that take more than 2 text boxes, but for this tutorial we’re only using meme templates with 2 text inputs to make it easier.
4. We choose a random template from this list to use as a basis for our meme (it’s actually a great way to serendipitously generate some interesting meme content).
5. We give the OpenAI API info about the functions it can create arguments for via the `functions` and `function_call` properties, which tell it to always return JSON arguments for our function, `generateMemeImage`

Great! But once we start generating memes, we will need a way to display them on our front end.

So let’s now create a Wasp Query. A Query works just like an Action, except it’s only for *reading* data.

Go to `src/server` and create a new file called `queries.ts`.

Next, in your `main.wasp` file add the following code:

```tsx
//...

query getAllMemes {
  fn: import { getAllMemes } from "@server/queries.js",
  entities: [Meme]
}
```

Then in your `queries.ts` file, add the `getAllMemes` function:

```tsx
import HttpError from '@wasp/core/HttpError.js';

import type { Meme } from '@wasp/entities';
import type { GetAllMemes } from '@wasp/queries/types';

export const getAllMemes: GetAllMemes<void, Meme[]> = async (_args, context) => {
  const memeIdeas = await context.entities.Meme.findMany({
    orderBy: { createdAt: 'desc' },
    include: { template: true },
  });

  return memeIdeas;
};
```

<a name='generate-client' href='#generate-client'></a>

### Client-Side Code

Now that we’ve got the `createMeme` and `getAllMemes` code implemented server-side, let’s hook it up to our client. 

Wasp makes it really easy to import the Operations we just created and call them on our front end.

You can do so by going to `src/client/pages/Home.tsx` and adding the following code to the top of the file:

```tsx
//...other imports...
import { useQuery } from '@wasp/queries';
import createMeme from '@wasp/actions/createMeme';
import getAllMemes from '@wasp/queries/getAllMemes';
import useAuth from '@wasp/auth/useAuth';

export function HomePage() {
  const [topics, setTopics] = useState(['']);
  const [audience, setAudience] = useState('');
  const [isMemeGenerating, setIsMemeGenerating] = useState(false);

  // 😎 😎 😎
  const { data: user } = useAuth();
  const { data: memes, isLoading, error } = useQuery(getAllMemes); 

  const handleGenerateMeme: FormEventHandler<HTMLFormElement> = async (e) => {
    e.preventDefault();
    if (!user) {
      history.push('/login');
      return;
    }
    if (topics.join('').trim().length === 0 || audience.length === 0) {
      alert('Please provide topic and audience');
      return;
    }
    try {
      setIsMemeGenerating(true);
      await createMeme({ topics, audience }); // <--- 😎 😎 😎
    } catch (error: any) {
      alert('Error generating meme: ' + error.message);
    } finally {
      setIsMemeGenerating(false);
    }
  };

//...
```

As you can see, we’ve imported `createMeme` and `getAllMemes` (😎). 

For `getAllMemes`, we wrap it in the `useQuery` hook so that we can fetch and cache the data. On the other hand, our `createMeme` Action gets called in `handleGenerateMeme` which we will call when submit our form.

Rather than adding code to the `Home.tsx` file piece-by-piece, here is the file with all the code to generate and display the memes. Go ahead and replace all of `Home.tsx` with this code and I’ll explain it in more detail below:

```tsx
import { useState, FormEventHandler } from 'react';
import { useQuery } from '@wasp/queries';
import createMeme from '@wasp/actions/createMeme';
import getAllMemes from '@wasp/queries/getAllMemes';
import useAuth from '@wasp/auth/useAuth';
import { useHistory } from 'react-router-dom';
import {
  AiOutlinePlusCircle,
  AiOutlineMinusCircle,
  AiOutlineRobot,
} from 'react-icons/ai';

export function HomePage() {
  const [topics, setTopics] = useState(['']);
  const [audience, setAudience] = useState('');
  const [isMemeGenerating, setIsMemeGenerating] = useState(false);

  const history = useHistory();
  const { data: user } = useAuth();
  const { data: memes, isLoading, error } = useQuery(getAllMemes);

  const handleGenerateMeme: FormEventHandler<HTMLFormElement> = async (e) => {
    e.preventDefault();
    if (!user) {
      history.push('/login');
      return;
    }
    if (topics.join('').trim().length === 0 || audience.length === 0) {
      alert('Please provide topic and audience');
      return;
    }
    try {
      setIsMemeGenerating(true);
      await createMeme({ topics, audience });
    } catch (error: any) {
      alert('Error generating meme: ' + error.message);
    } finally {
      setIsMemeGenerating(false);
    }
  };

  const handleDeleteMeme = async (id: string) => {
    //...
  };

  if (isLoading) return 'Loading...';
  if (error) return 'Error: ' + error;

  return (
    <div className='p-4'>
      <h1 className='text-3xl font-bold mb-4'>Welcome to Memerator!</h1>
      <p className='mb-4'>Start generating meme ideas by providing topics and intended audience.</p>
      <form onSubmit={handleGenerateMeme}>
        <div className='mb-4 max-w-[500px]'>
          <label htmlFor='topics' className='block font-bold mb-2'>
            Topics:
          </label>
          {topics.map((topic, index) => (
            <input
              key={index}
              type='text'
              id='topics'
              value={topic}
              onChange={(e) => {
                const updatedTopics = [...topics];
                updatedTopics[index] = e.target.value;
                setTopics(updatedTopics);
              }}
              className='p-1 mr-1 mb-1 border rounded text-lg focus:outline-none focus:ring-2 focus:ring-primary-600 focus:border-transparent'
            />
          ))}
          <div className='flex items-center my-2 gap-1'>
            <button
              type='button'
              onClick={() => topics.length < 3 && setTopics([...topics, ''])}
              className='flex items-center gap-1 bg-primary-200 hover:bg-primary-300 border-2 text-black text-xs py-1 px-2 rounded'
            >
              <AiOutlinePlusCircle /> Add Topic
            </button>
            {topics.length > 1 && (
              <button
                onClick={() => setTopics(topics.slice(0, -1))}
                className='flex items-center gap-1 bg-red-500 hover:bg-red-700 border-2 text-white text-xs py-1 px-2 rounded'
              >
                <AiOutlineMinusCircle /> Remove Topic
              </button>
            )}
          </div>
        </div>
        <div className='mb-4'>
          <label htmlFor='audience' className='block font-bold mb-2'>
            Intended Audience:
          </label>
          <input
            type='text'
            id='audience'
            value={audience}
            onChange={(e) => setAudience(e.target.value)}
            className='p-1 border rounded text-lg focus:outline-none focus:ring-2 focus:ring-primary-600 focus:border-transparent'
          />
        </div>
        <button
          type='submit'
          className={`flex items-center gap-1 bg-primary-200 hover:bg-primary-300 border-2 text-black text-sm font-bold py-1 px-2 rounded ${
            isMemeGenerating ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer'
          } $}`}
        >
          <AiOutlineRobot />
          {!isMemeGenerating ? 'Generate Meme' : 'Generating...'}
        </button>
      </form>

      {!!memes && memes.length > 0 ? (
        memes.map((memeIdea) => (
          <div key={memeIdea.id} className='mt-4 p-4 bg-gray-100 rounded-lg'>
            <img src={memeIdea.url} width='500px' />
            <div className='flex flex-col items-start mt-2'>
              <div>
                <span className='text-sm text-gray-700'>Topics: </span>
                <span className='text-sm italic text-gray-500'>{memeIdea.topics}</span>
              </div>
              <div>
                <span className='text-sm text-gray-700'>Audience: </span>
                <span className='text-sm italic text-gray-500'>{memeIdea.audience}</span>
              </div>
            </div>
            {/* TODO: implement edit and delete meme features */}
          </div>
        ))
      ) : (
        <div className='flex justify-center mt-5'> :( no memes found</div>
      )}
    </div>
  );
}
```

There are two things I want to point out about this code:

1. The `useQuery` hook calls our `getAllMemes` Query when the component mounts. It also caches the result for us, as well as automatically re-fetching whenever we add a new Meme to our DB via `createMeme`. This means our page will reload automatically whenever a new meme is generated.
2. The `useAuth` hook allows us to fetch info about our logged in user. If the user isn’t logged in, we force them to do so before they can generate a meme.

These are really cool Wasp features that make your life as a developer a lot easier 🙂

So go ahead now and try and generate a meme. Here’s the one I just generated:

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/c2ypm9n0uyra47mp5g85.png)

Haha. Pretty good!

Now wouldn’t it be cool though if we could edit and delete our memes? And what if we could expand the set of meme templates for our generator to use? Wouldn’t that be cool, too?

Yes, it would be. So let’s do that.

<a name="part-two" href="#part-two"></a>

# Part 2.

So we’ve got ourselves a really good basis for an app at this point.

We’re using [OpenAI’s function calling feature](https://platform.openai.com/docs/guides/gpt/function-calling) to explain a function to GPT, and get it to return results for us in a format we can use to call that function.

This allows us to be certain GPT’s result will be usable in further parts of our application and opens up the door to creating AI agents.

If you think about it, we’ve basically got ourselves a really simple Meme generating “agent”. How cool is that?!

<a name="templates" href="#templates"></a>

## Fetching & Updating Templates with Cron Jobs

To be able to generate our meme images via [ImgFlip’s API](https://imgflip.com/api), we have to choose and send a meme template `id` to the API, along with the text arguments we want to fill it in with.

For example, the `Grandma Finds Internet` meme template has the following `id`:

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/zoo4uainls9jxq7fzysv.png)

But the only way for us to get available meme templates from ImgFlip is to send a `GET` request to
[https://api.imgflip.com/get_memes](https://api.imgflip.com/get_memes). And according to ImgFlip, the **/get-memes** endpoint works like this:

> Gets an array of popular memes that may be captioned with this API. The size of this array and the order of memes may change at any time. When this description was written, it returned 100 memes ordered by how many times they were captioned in the last 30 days
> 

So it returns a list of the top 100 memes from the last 30 days. And as this is always changing, we can run a daily cron job to fetch the list and update our database with any new templates that don’t already exist in it.

We know this will work because the ImgFlip docs for the **/caption-image** endpoint — which we use to create a meme image — says this:

> *key:* template_id
*value:* A template ID as returned by the **get_memes** response. Any ID that was ever returned from the **get_memes** response should work for this parameter…
> 

Awesome!

<a name="cron" href="#cron"></a>

### Defining our Daily Cron Job

Now, to create an automatically [recurring cron job in Wasp](https://wasp-lang.dev/docs/advanced/jobs) is really easy. 

First, go to your `main.wasp` file and add:

```tsx
job storeMemeTemplates {
  executor: PgBoss,
  perform: {
    fn: import { fetchAndStoreMemeTemplates } from "@server/workers.js",
  },
  schedule: {
    // daily at 7 a.m.
    cron: "0 7 * * *" 
  },
  entities: [Template],
}
```

This is telling Wasp to run the `fetchAndStoreMemeTemplates` function every day at 7 a.m.

Next, create a new file in `src/server` called `workers.ts` and add the function:

```tsx
import axios from 'axios';

export const fetchAndStoreMemeTemplates = async (_args: any, context: any) => {
  console.log('.... ><><>< get meme templates cron starting ><><>< ....');

  try {
    const response = await axios.get('https://api.imgflip.com/get_memes');

    const promises = response.data.data.memes.map((meme: any) => {
      return context.entities.Template.upsert({
        where: { id: meme.id },
        create: {
          id: meme.id,
          name: meme.name,
          url: meme.url,
          width: meme.width,
          height: meme.height,
          boxCount: meme.box_count,
        },
        update: {},
      });
    });

    await Promise.all(promises);
  } catch (error) {
    console.error('error fetching meme templates: ', error);
  }
};
```

You can see that we send a `GET` request to the proper endpoint, then we loop through the array of memes it returns to us add any new templates to the database.

Notice that we use Prisma’s `upsert` method here. This allows us to create a new entity in the database if it doesn’t already exist. If it does, we don’t do anything, which is why `update` is left blank.

We use `[Promise.all()` to call that array of promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all) correctly.

<a name="cron-test" href="#cron-test"></a>

### Testing

Now, assuming you’ve got your app running with `wasp start`, you will see the cron job run in the console every day at 7 a.m.

If you want to test that the cron job is working correctly, you could run it on a faster schedule. Let’s try that now by changing it in our `main.wasp` file to run every minute:

```tsx
//...
  schedule: {
    // runs every minute.
    cron: "* * * * *" 
  },
```

First, your terminal where you ran `wasp start` to start your app should output the following:

```bash
[Server]  🔍 Validating environment variables...
[Server]  🚀 "Username and password" auth initialized
[Server]  Starting pg-boss...
[Server]  pg-boss started!
[Server]  Server listening on port 3001
```

…followed shortly after by:

```bash
[Server]  .... ><><>< get meme templates cron starting ><><>< ....
```

Great. We’ve got an automatically recurring cron job going.

You can check your database for saved templates by opening another terminal window and running:

```bash
wasp db studio 
```

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/yldqge5186hegvzjyo30.png)

<a name="edit" href="#edit"></a>

## Editing Memes

Unfortunately, sometimes GPT’s results have some mistakes. Or sometimes the idea is really good, but we want to further modify it to make it even better.

Well, that’s pretty simple for us to do since we can just make another call to [ImgFlip’s API](https://imgflip.com/api).

So let’s set do that by setting up a dedicated page where we:

- fetch that specific meme based on its `id`
- display a form to allow the user to edit the meme `text`
- send that info to a server-side Action which calls the ImgFlip API, generates a new image URL, and updates our `Meme` entity in the DB.

<a name="server" href="#server"></a>

### Server-Side Code

To make sure we can fetch the individual meme we want to edit, we first need to set up a Query that does this.

Go to your `main.wasp` file and add this Query declaration:

```tsx
query getMeme {
  fn: import { getMeme } from "@server/queries.js",
  entities: [Meme]
}
```


Now go to `src/server/queries.ts` and add the following function:

```tsx
import type { Meme, Template } from '@wasp/entities';
import type { GetAllMemes, GetMeme } from '@wasp/queries/types';

type GetMemeArgs = { id: string };
type GetMemeResult = Meme & { template: Template };

//...

export const getMeme: GetMeme<GetMemeArgs, GetMemeResult> = async ({ id }, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  const meme = await context.entities.Meme.findUniqueOrThrow({
    where: { id: id },
    include: { template: true },
  });

  return meme;
};
```

We’re just fetching the single meme based on its `id` from the database.

We’re also including the related meme `Template` so that we have access to its `id` as well, because we need to send this to the ImgFlip API too.

Pretty simple!

Now let’s create our `editMeme` action by going to our `main.wasp` file and adding the following Action:

```tsx
//...

action editMeme {
  fn: import { editMeme } from "@server/actions.js",
  entities: [Meme, Template, User]
}
```

Next, move over to the `server/actions.ts` file and let’s add the following server-side function:

```tsx
//... other imports
import type { EditMeme } from '@wasp/actions/types';

//... other types
type EditMemeArgs = Pick<Meme, 'id' | 'text0' | 'text1'>;

export const editMeme: EditMeme<EditMemeArgs, Meme> = async ({ id, text0, text1 }, context) => {
  if (!context.user) {
    throw new HttpError(401, 'You must be logged in');
  }

  const meme = await context.entities.Meme.findUniqueOrThrow({
    where: { id: id },
    include: { template: true },
  });

  if (!context.user.isAdmin && meme.userId !== context.user.id) {
    throw new HttpError(403, 'You are not the creator of this meme');
  }

  const memeUrl = await generateMemeImage({
    templateId: meme.template.id,
    text0: text0,
    text1: text1,
  });

  const newMeme = await context.entities.Meme.update({
    where: { id: id },
    data: {
      text0: text0,
      text1: text1,
      url: memeUrl,
    },
  });

  return newMeme;
};
```

As you can see, this function expects the `id` of the already existing meme, along with the new `text` boxes. That’s because we’re letting the user manually input/edit the text that GPT generated, rather than making another request the the OpenAI API.

Next, we look for that specific meme in our database, and if we don’t find it we throw an error (`findUniqueOrThrow`).

We check to make sure that that meme belongs to the user that is currently making the request, because we don’t want a different user to edit a meme that doesn’t belong to them.

Then we send the template `id` of that meme along with the new text to our previously created `generateMemeImage` function. This function calls the ImgFlip API and returns the url of the newly created meme image.

We then `update` the database to save the new URL to our Meme.

Awesome!

<a name="client" href="#client"></a>

### Client-Side Code

Let’s start by adding a new route and page to our `main.wasp` file:

```tsx
//...

route EditMemeRoute { path: "/meme/:id", to: EditMemePage }
page EditMemePage {
  component: import { EditMemePage } from "@client/pages/EditMemePage",
  authRequired: true
}
```

There are two important things to notice:

1. the path includes the `:id` parameter, which means we can access page for any meme in our database by going to, e.g. `memerator.com/meme/5`
2. by using the `authRequired` option, we tell Wasp to automatically block this page from unauthorized users. Nice!

Now, create this page by adding a new file called `EditMemePage.tsx` to `src/client/pages`. Add the following code:

```tsx
import { useState, useEffect, FormEventHandler } from 'react';
import { useQuery } from '@wasp/queries';
import editMeme from '@wasp/actions/editMeme';
import getMeme from '@wasp/queries/getMeme';
import { useParams } from 'react-router-dom';
import { AiOutlineEdit } from 'react-icons/ai';

export function EditMemePage() {
  // http://localhost:3000/meme/573f283c-24e2-4c45-b6b9-543d0b7cc0c7
  const { id } = useParams<{ id: string }>();

  const [text0, setText0] = useState('');
  const [text1, setText1] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  const { data: meme, isLoading: isMemeLoading, error: memeError } = useQuery(getMeme, { id: id });

  useEffect(() => {
    if (meme) {
      setText0(meme.text0);
      setText1(meme.text1);
    }
  }, [meme]);

  const handleSubmit: FormEventHandler<HTMLFormElement> = async (e) => {
    e.preventDefault();
    try {
      setIsLoading(true);
      await editMeme({ id, text0, text1 });
    } catch (error: any) {
      alert('Error generating meme: ' + error.message);
    } finally {
      setIsLoading(false);
    }
  };

  if (isMemeLoading) return 'Loading...';
  if (memeError) return 'Error: ' + memeError.message;

  return (
    <div className='p-4'>
      <h1 className='text-3xl font-bold mb-4'>Edit Meme</h1>
      <form onSubmit={handleSubmit}>
        <div className='flex gap-2 items-end'>
          <div className='mb-2'>
            <label htmlFor='text0' className='block font-bold mb-2'>
              Text 0:
            </label>
            <textarea
              id='text0'
              value={text0}
              onChange={(e) => setText0(e.target.value)}
              className='border rounded px-2 py-1'
            />
          </div>
          <div className='mb-2'>
            <label htmlFor='text1' className='block font-bold mb-2'>
              Text 1:
            </label>

            <div className='flex items-center mb-2'>
              <textarea
                id='text1'
                value={text1}
                onChange={(e) => setText1(e.target.value)}
                className='border rounded px-2 py-1'
              />
            </div>
          </div>
        </div>

        <button
          type='submit'
          className={`flex items-center gap-1 bg-primary-200 hover:bg-primary-300 border-2 text-black text-sm py-1 px-2 rounded ${
            isLoading ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer'
          } $}`}
        >
          <AiOutlineEdit />
          {!isLoading ? 'Save Meme' : 'Saving...'}
        </button>
      </form>
      {!!meme && (
        <div className='mt-4  mb-2 bg-gray-100 rounded-lg p-4'>
          <img src={meme.url} width='500px' />
          <div className='flex flex-col items-start mt-2'>
            <div>
              <span className='text-sm text-gray-700'>Topics: </span>
              <span className='text-sm italic text-gray-500'>{meme.topics}</span>
            </div>
            <div>
              <span className='text-sm text-gray-700'>Audience: </span>
              <span className='text-sm italic text-gray-500'>{meme.audience}</span>
            </div>
            <div>
              <span className='text-sm text-gray-700'>ImgFlip Template: </span>
              <span className='text-sm italic text-gray-500'>{meme.template.name}</span>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
```

Some things to notice here are:

1. because we’re using dynamic routes (`/meme/:id`), we pull the URL paramater `id` from the url with `useParams` hook.
2. we then pass that `id` within the `getMemes` Query to fetch that specific meme to edit: `useQuery(getMeme, { id: id })`
    1. remember, our server-side action depends on this `id` in order to fetch the meme from our database

The rest of the page is just our form for calling the `editMeme` Action, as well as displaying the meme we want to edit.

That’s great!

Now that we have that `EditMemePage`, we need a way to navigate to it from the home page.

To do that, go back to the `Home.tsx` file, add the following imports at the top, and find the comment that says `{/* TODO: implement edit and delete meme features */}` and replace it with the following code: 

```tsx
import { Link } from '@wasp/router';
import { AiOutlineEdit } from 'react-icons/ai';

//...

{user && (user.isAdmin || user.id === memeIdea.userId) && (
  <div className='flex items-center mt-2'>
    <Link key={memeIdea.id} params={{ id: memeIdea.id }} to={`/meme/:id`}>
      <button className='flex items-center gap-1 bg-primary-200 hover:bg-primary-300 border-2 text-black text-xs py-1 px-2 rounded'>
        <AiOutlineEdit />
        Edit Meme
      </button>
    </Link>
    {/* TODO: add delete meme functionality */}
  </div>
)}
```

What’s really cool about this, is that Wasp’s `Link` component will give you [type-safe routes](https://wasp-lang.dev/docs/advanced/links), by making sure you’re following the pattern you defined in your `main.wasp` file.

And with that, so long as the authenticated user was the creator of the meme (or is an admin), the `Edit Meme` button will show up and direct the user to the `EditMemePage`

Give it a try now. It should look like this:

<iframe width="560" height="315" src="https://www.youtube.com/embed/ymSr2eRXz9c?si=BI9s2WEHnRiPtC3G" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

<a name="delete" href="#delete"></a>

## Deleting Memes

Ok. When I initially started writing this tutorial, I thought I’d also explain how to add `delete` meme functionality to the app as well.

But seeing as we’ve gotten this far, and as the entire two-part tutorial is pretty long, I figured you should be able to implement yourself by this point.

So I’ll leave you guide as to how to implement it yourself. Think of it as a bit of homework:

1. define the `deleteMeme` Action in your `main.wasp` file
2. export the async function from the `actions.ts` file
3. import the Action in your client-side code
4. create a button which takes the meme’s `id` as an argument in your `deleteMeme` Action.

If you get stuck, you can use the `editMeme` section as a guide. Or you can check out the [finished app’s GitHub repo](https://github.com/vincanger/memerator/) for the completed code!

<a name="conclusion" href="#conclusion"></a>

# Conclusion

There you have it! Your own instant meme generator 🤖😆

BTW, If you found this useful, **please show us your support by [giving us a star on GitHub](https://github.com/wasp-lang/wasp)**! It will help us continue to make more stuff just like it. 

![https://res.cloudinary.com/practicaldev/image/fetch/s--tnDxibZC--/c_limit%2Cf_auto%2Cfl_progressive%2Cq_66%2Cw_800/https://res.cloudinary.com/practicaldev/image/fetch/s--OCpry2p9--/c_limit%252Cf_auto%252Cfl_progressive%252Cq_66%252Cw_800/https://dev-to-uploads.s3.amazonaws.com/uploads/articles/bky8z46ii7ayejprrqw3.gif](https://res.cloudinary.com/practicaldev/image/fetch/s--tnDxibZC--/c_limit%2Cf_auto%2Cfl_progressive%2Cq_66%2Cw_800/https://res.cloudinary.com/practicaldev/image/fetch/s--OCpry2p9--/c_limit%252Cf_auto%252Cfl_progressive%252Cq_66%252Cw_800/https://dev-to-uploads.s3.amazonaws.com/uploads/articles/bky8z46ii7ayejprrqw3.gif)