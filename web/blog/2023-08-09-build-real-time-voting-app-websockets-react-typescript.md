---
title: 'Build a real-time voting app with WebSockets, React & TypeScript 🔌⚡️'
authors: [vinny]
image: /img/websockets-app/websockets-banner.png
tags: [wasp, websockets, react, typescript, real-time, node, express]
---
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'


## TL;DR

WebSockets allow your app to have “real time” features, where updates are instant because they’re passed on an open, two-way channel. This is a different from CRUD apps, which usually use HTTP requests that must establish a connection, send a request, receive a response, and then close the connection.

<ImgWithCaption
  source="img/websockets-app/Untitled.png"
  width="550px"
/>

To use WebSockets in your React app, you’ll need a dedicated server, such as an ExpressJS app with NodeJS, in order to maintain a persistent connection.

Unfortunately, serverless solutions (e.g. NextJS, AWS lambda) don’t natively support WebSockets. Bummer. 😞 

Why not? Well, serverless services turn on and off depending on if a request is coming in. With WebSockets, we need this “always on” connection that only a dedicated server can provide (although you can pay for third-party services as a workaround).

Luckily, we’re going to talk about two great ways you can implement them:

1. Implementing and configuring it yourself with React, NodeJS, and Socket.IO
2. By using [Wasp](https://wasp-lang.dev), a full-stack React-NodeJS framework, to configure and integrate Socket.IO into your app for you.

These methods allow you to build fun stuff, like this instantly updating “voting with friends” app we built here (check out the [GitHub repo for it](https://github.com/vincanger/websockets-wasp)):

<!-- [https://www.youtube.com/watch?v=Twy-2P0Co6M](https://www.youtube.com/watch?v=Twy-2P0Co6M) -->
<iframe width="560" height="315" src="https://www.youtube.com/embed/Twy-2P0Co6M" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Why WebSockets?

So, imagine you're at a party sending text messages to a friend to tell them what food to bring. 

Now, wouldn’t it be easier if you called your friend on the phone so you could talk constantly,  instead of sending sporadic messages? That's pretty much what WebSockets are in the world of web applications. 

For example, traditional HTTP requests (e.g. CRUD/RESTful) are like those text messages — your app has to **ask the server** every time it wants new information, just like you had to send a text message to your friend every time you thought of food for your party. 

But with WebSockets, once a connection is established, it **remains open** for constant, two-way communication, so the server can send new information to your app the instant it becomes available, even if the client didn’t ask for it. 

This is perfect for real-time applications like chat apps, game servers, or when you're keeping track of stock prices. For example, apps like Google Docs, Slack, WhatsApp, Uber, Zoom, and Robinhood all use WebSockets to power their real-time communication features.

![https://media3.giphy.com/media/26u4hHj87jMePiO3u/giphy.gif?cid=7941fdc6hxgjnub1rcs80udcj652956fwmm4qhxsmk6ldxg7&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media3.giphy.com/media/26u4hHj87jMePiO3u/giphy.gif?cid=7941fdc6hxgjnub1rcs80udcj652956fwmm4qhxsmk6ldxg7&ep=v1_gifs_search&rid=giphy.gif&ct=g)

So remember, when your app and server have a lot to talk about, go for WebSockets and let the conversation flow freely!

## How WebSockets Work

If you want real-time capabilities in your app, you don’t always need WebSockets. You can implement similar functionality by using resource-heavy processes, such as:

1. long-polling, e.g. running `setInterval` to periodically hit the server and check for updates.
2. one-way “server-sent events”, e.g. keeping a unidirectional server-to-client connection open to receive new updates from the server only.

<ImgWithCaption
  source="img/websockets-app/Untitled 1.png"
  width="550px"
  caption="1. HTTP handshake, 2. two-way instant communication, 3. close connection"
/>

WebSockets, on the other hand, provide a two-way (aka “full-duplex”) communication channel between the client and server. 

Once established via an HTTP “handshake”, the server and client can freely exchange information instantly before the connection is finally closed by either side.

Although introducing WebSockets does add complexity due to asynchronous and event-driven components, choosing the right libraries and frameworks can make it easy.

In the sections below, we will show you two ways to implement WebSockets into a React-NodeJS app:

1. Configuring it yourself alongside your own standalone Node/ExpressJS server
2. Letting Wasp, a full-stack framework with superpowers, easily configure it for you

## Adding WebSockets Support in a React-NodeJS App

### What You Shouldn’t Use: Serverless Architecture

But first, here’s a “heads up” for you: despite being a great solution for certain use-cases, serverless solutions are **not** the right tool for this job.

That means, popular frameworks and infrastructure, like NextJS and AWS Lambda, do not support WebSockets integration out-of-the-box. 

<!-- [https://www.youtube.com/watch?v=e5Cye4pIFeA](https://www.youtube.com/watch?v=e5Cye4pIFeA) -->
<iframe width="560" height="315" src="https://www.youtube.com/embed/e5Cye4pIFeA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

Instead of running on a dedicated, traditional server, such solutions utilize serverless functions (also known as lambda functions), which are designed to execute and complete a task as soon as a request comes in. It’s as if they “turn on” when the request comes in, and then “turn off” once it’s completed.

This serverless architecture is not ideal for keeping a WebSocket connection alive because we want a persistent, “always-on” connection.

That’s why you need a “serverful” architecture if you want to build real-time apps. And although there is a workaround to getting WebSockets on a serverless architecture, [like using third-party services](https://vercel.com/guides/do-vercel-serverless-functions-support-websocket-connections), this has a number of drawbacks:

- **Cost:** these services exist as subscriptions and can get costly as your app scales
- **Limited Customization:** you’re using a pre-built solution, so you have less control
- **Debugging:** fixing errors gets more difficult, as your app is not running locally

<ImgWithCaption
  source="img/websockets-app/Untitled 2.png"
  width="550px"
/>

### Using ExpressJS with Socket.IO  — Complex/Customizable Method

Okay, let's start with the first, more traditional approach: creating a dedicated server for your client to establish a two-way communication channel with.

:::note
👨‍💻 If you want to code along you can follow the instructions below. Alternatively, if you just want to see the finished React-NodeJS full-stack app, check out the [github repo here](https://github.com/vincanger/websockets-react)
:::

In this exampple, we’ll be using [ExpressJS](https://expressjs.com/) with the [Socket.IO](http://Socket.io) library. Although there are others out there, Socket.IO is a great library that makes working with WebSockets in NodeJS [easier](https://socket.io/docs/v4/).

If you want to code along, first clone the `start` branch:

```bash
git clone --branch start https://github.com/vincanger/websockets-react.git
```

You’ll notice that inside we have two folders:

- 📁 `ws-client` for our React app
- 📁 `ws-server` for our ExpressJS/NodeJS server

Let’s `cd` into the server folder and install the dependencies:

```bash
cd ws-server && npm install
```

We also need to install the types for working with typescript:

```bash
npm i --save-dev @types/cors
```

Now run the server, using the `npm start` command in your terminal. 

You should see `listening on *:8000` printed to the console!

At the moment, this is what our `index.ts` file looks like:

```tsx
import cors from 'cors';
import express from 'express';

const app = express();
app.use(cors({ origin: '*' }));
const server = require('http').createServer(app);

app.get('/', (req, res) => {
  res.send(`<h1>Hello World</h1>`);
});

server.listen(8000, () => {
  console.log('listening on *:8000');
});
```

There’s not much going on here, so let’s install the [Socket.IO](http://Socket.IO) package and start adding WebSockets to our server!

First, let’s kill the server with `ctrl + c` and then run: 

```bash
npm install socket.io
```

Let’s go ahead and replace the `index.ts` file with the following code. I know it’s a lot of code, so I’ve left a bunch of comments that explain what’s going on ;):

```tsx
import cors from 'cors';
import express from 'express';
import { Server, Socket } from 'socket.io';

type PollState = {
  question: string;
  options: {
    id: number;
    text: string;
    description: string;
    votes: string[];
  }[];
};
interface ClientToServerEvents {
  vote: (optionId: number) => void;
  askForStateUpdate: () => void;
}
interface ServerToClientEvents {
  updateState: (state: PollState) => void;
}
interface InterServerEvents { }
interface SocketData {
  user: string;
}

const app = express();
app.use(cors({ origin: 'http://localhost:5173' })); // this is the default port that Vite runs your React app on
const server = require('http').createServer(app);
// passing these generic type parameters to the `Server` class
// ensures data flowing through the server are correctly typed.
const io = new Server<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents,
  SocketData
>(server, {
  cors: {
    origin: 'http://localhost:5173',
    methods: ['GET', 'POST'],
  },
});

// this is middleware that Socket.IO uses on initiliazation to add
// the authenticated user to the socket instance. Note: we are not
// actually adding real auth as this is beyond the scope of the tutorial
io.use(addUserToSocketDataIfAuthenticated);

// the client will pass an auth "token" (in this simple case, just the username)
// to the server on initialize of the Socket.IO client in our React App
async function addUserToSocketDataIfAuthenticated(socket: Socket, next: (err?: Error) => void) {
  const user = socket.handshake.auth.token;
  if (user) {
    try {
      socket.data = { ...socket.data, user: user };
    } catch (err) {}
  }
  next();
}

// the server determines the PollState object, i.e. what users will vote on
// this will be sent to the client and displayed on the front-end
const poll: PollState = {
  question: "What are eating for lunch ✨ Let's order",
  options: [
    {
      id: 1,
      text: 'Party Pizza Place',
      description: 'Best pizza in town',
      votes: [],
    },
    {
      id: 2,
      text: 'Best Burger Joint',
      description: 'Best burger in town',
      votes: [],
    },
    {
      id: 3,
      text: 'Sus Sushi Place',
      description: 'Best sushi in town',
      votes: [],
    },
  ],
};

io.on('connection', (socket) => {
  console.log('a user connected', socket.data.user);

	// the client will send an 'askForStateUpdate' request on mount
	// to get the initial state of the poll
  socket.on('askForStateUpdate', () => {
    console.log('client asked For State Update');
    socket.emit('updateState', poll);
  });

  socket.on('vote', (optionId: number) => {
    // If user has already voted, remove their vote.
    poll.options.forEach((option) => {
      option.votes = option.votes.filter((user) => user !== socket.data.user);
    });
    // And then add their vote to the new option.
    const option = poll.options.find((o) => o.id === optionId);
    if (!option) {
      return;
    }
    option.votes.push(socket.data.user);
		// Send the updated PollState back to all clients
    io.emit('updateState', poll);
  });

  socket.on('disconnect', () => {
    console.log('user disconnected');
  });
});

server.listen(8000, () => {
  console.log('listening on *:8000');
});
```

Great, start the server again with `npm start` and let’s add the [Socket.IO](http://Socket.IO) client to the front-end.

`cd` into the `ws-client` directory and run

```bash
cd ../ws-client && npm install
```

Next, start the development server with `npm run dev` and you should see the hardcoded starter app in your browser:

<ImgWithCaption
  source="img/websockets-app/Untitled 3.png"
  width="550px"
/>

You may have noticed that poll does not match the `PollState` from our server. We need to install the [Socket.IO](http://Socket.IO) client and set it all up in order start our real-time communication and get the correct poll from the server.

Go ahead and kill the development server with `ctrl + c` and run:

```bash
npm install socket.io-client
```

Now let’s create a hook that initializes and returns our WebSocket client after it establishes a connection. To do that, create a new file in `./ws-client/src` called `useSocket.ts`:

```tsx
import { useState, useEffect } from 'react';
import socketIOClient, { Socket } from 'socket.io-client';

export type PollState = {
  question: string;
  options: {
    id: number;
    text: string;
    description: string;
    votes: string[];
  }[];
};
interface ServerToClientEvents {
  updateState: (state: PollState) => void;
}
interface ClientToServerEvents {
  vote: (optionId: number) => void;
  askForStateUpdate: () => void;
}

export function useSocket({endpoint, token } : { endpoint: string, token: string }) {
  // initialize the client using the server endpoint, e.g. localhost:8000
	// and set the auth "token" (in our case we're simply passing the username
	// for simplicity -- you would not do this in production!)
	// also make sure to use the Socket generic types in the reverse order of the server!
	const socket: Socket<ServerToClientEvents, ClientToServerEvents>  = socketIOClient(endpoint,  {
    auth: {
      token: token
    }
  }) 
  const [isConnected, setIsConnected] = useState(false);

  useEffect(() => {
    console.log('useSocket useEffect', endpoint, socket)

    function onConnect() {
      setIsConnected(true)
    }

    function onDisconnect() {
      setIsConnected(false)
    }

    socket.on('connect', onConnect)
    socket.on('disconnect', onDisconnect)

    return () => {
      socket.off('connect', onConnect)
      socket.off('disconnect', onDisconnect)
    }
  }, [token]);

	// we return the socket client instance and the connection state
  return {
    isConnected,
    socket,
  };
}
```

Now let’s go back to our main `App.tsx` page and replace it with the following code (again I’ve left comments to explain):

```tsx
import { useState, useMemo, useEffect } from 'react';
import { Layout } from './Layout';
import { Button, Card } from 'flowbite-react';
import { useSocket } from './useSocket';
import type { PollState } from './useSocket';

const App = () => {
	// set the PollState after receiving it from the server
  const [poll, setPoll] = useState<PollState | null>(null);
	 
	// since we're not implementing Auth, let's fake it by
	// creating some random user names when the App mounts
  const randomUser = useMemo(() => {
    const randomName = Math.random().toString(36).substring(7);
    return `User-${randomName}`;
  }, []);
	
	// 🔌⚡️ get the connected socket client from our useSocket hook! 
  const { socket, isConnected } = useSocket({ endpoint: `http://localhost:8000`, token: randomUser });

  const totalVotes = useMemo(() => {
    return poll?.options.reduce((acc, option) => acc + option.votes.length, 0) ?? 0;
  }, [poll]);

	// every time we receive an 'updateState' event from the server
	// e.g. when a user makes a new vote, we set the React's state
	// with the results of the new PollState 
  socket.on('updateState', (newState: PollState) => {
    setPoll(newState);
  });

  useEffect(() => {
    socket.emit('askForStateUpdate');
  }, []);

  function handleVote(optionId: number) {
    socket.emit('vote', optionId);
  }

  return (
    <Layout user={randomUser}>
      <div className='w-full max-w-2xl mx-auto p-8'>
        <h1 className='text-2xl font-bold'>{poll?.question ?? 'Loading...'}</h1>
        <h2 className='text-lg italic'>{isConnected ? 'Connected ✅' : 'Disconnected 🛑'}</h2>
        {poll && <p className='leading-relaxed text-gray-500'>Cast your vote for one of the options.</p>}
        {poll && (
          <div className='mt-4 flex flex-col gap-4'>
            {poll.options.map((option) => (
              <Card key={option.id} className='relative transition-all duration-300 min-h-[130px]'>
                <div className='z-10'>
                  <div className='mb-2'>
                    <h2 className='text-xl font-semibold'>{option.text}</h2>
                    <p className='text-gray-700'>{option.description}</p>
                  </div>
                  <div className='absolute bottom-5 right-5'>
                    {randomUser && !option.votes.includes(randomUser) ? (
                      <Button onClick={() => handleVote(option.id)}>Vote</Button>
                    ) : (
                      <Button disabled>Voted</Button>
                    )}
                  </div>
                  {option.votes.length > 0 && (
                    <div className='mt-2 flex gap-2 flex-wrap max-w-[75%]'>
                      {option.votes.map((vote) => (
                        <div
                          key={vote}
                          className='py-1 px-3 bg-gray-100 rounded-lg flex items-center justify-center shadow text-sm'
                        >
                          <div className='w-2 h-2 bg-green-500 rounded-full mr-2'></div>
                          <div className='text-gray-700'>{vote}</div>
                        </div>
                      ))}
                    </div>
                  )}
                </div>
                <div className='absolute top-5 right-5 p-2 text-sm font-semibold bg-gray-100 rounded-lg z-10'>
                  {option.votes.length} / {totalVotes}
                </div>
                <div
                  className='absolute inset-0 bg-gradient-to-r from-yellow-400 to-orange-500 opacity-75 rounded-lg transition-all duration-300'
                  style={{
                    width: `${totalVotes > 0 ? (option.votes.length / totalVotes) * 100 : 0}%`,
                  }}
                ></div>
              </Card>
            ))}
          </div>
        )}
      </div>
    </Layout>
  );
};
export default App;
```

Go ahead now and start the client with `npm run dev`. Open another terminal window/tab, `cd` into the `ws-server` directory and run `npm start`.

If we did that correctly, we should be seeing our finished, working, REAL TIME app! 🙂

It looks and works great if you open it up in two or three browser tabs. Check it out:

<ImgWithCaption
  source="img/websockets-app/Untitled.gif"
  width="550px"
/>

Nice!

So we’ve got the core functionality here, but as this is just a demo, there are a couple very important pieces missing that make this app unusable in production.

Mainly, we’re creating a random fake user each time the app mounts. You can check this by refreshing the page and voting again. You’ll see the votes just add up, as we’re creating a new random user each time. We don’t want that! 

We should instead be authenticating and persisting a session for a user that’s registered in our database. But another problem: we don’t even have a database at all in this app!

You can start to see the how the complexity add ups for even just a simple voting feature

Luckily, our next solution, Wasp, has integrated Authentication and Database Management. Not to mention, it also takes care of a lot of the WebSockets configuration for us.

So let’s go ahead and give that a go! 

### Implementing WebSockets with Wasp — Fast/Zero Config Method

Because Wasp is an innovative full-stack framework, it makes building React-NodeJS apps quick and developer-friendly. 

Wasp has lots of time-saving features, including WebSocket support via [Socket.IO](http://socket.io/), Authentication, Database Management, and Full-stack type-safety out-of-the box.

<!-- [https://twitter.com/WaspLang/status/1673742264873500673?s=20](https://twitter.com/WaspLang/status/1673742264873500673?s=20) -->

Wasp can take care of all this heavy lifting for you because of its use of a config file, which you can think of like a set of instructions that the Wasp compiler uses to help glue your app together.

To see it in action, let's implement WebSocket communication using Wasp by following these steps 

:::tip
If you just want to see finished app’s code, you can check out the [GitHub repo here](https://github.com/vincanger/websockets-wasp)
:::

1. Install Wasp globally by running the following command in your terminal:

```bash
curl -sSL [https://get.wasp-lang.dev/installer.sh](https://get.wasp-lang.dev/installer.sh) | sh 
```

If you want to code along, first clone the `start` branch of the example app:

```bash
git clone --branch start https://github.com/vincanger/websockets-wasp.git
```

You’ll notice that the structure of the Wasp app is split:

- 🐝 a `main.wasp` config file exists at the root
- 📁 `src/client` is our directory for our React files
- 📁 `src/server` is our directory for our ExpressJS/NodeJS functions

Let’s start out by taking a quick look at our `main.wasp` file.

```jsx
app whereDoWeEat {
  wasp: {
    version: "^0.11.0"
  },
  title: "where-do-we-eat",
  client: {
    rootComponent: import { Layout } from "@client/Layout.jsx",
  },
	// 🔐 this is how we get auth in our app.
  auth: {
    userEntity: User,
    onAuthFailedRedirectTo: "/login",
    methods: {
      usernameAndPassword: {}
    }
  },
  dependencies: [
    ("flowbite", "1.6.6"),
    ("flowbite-react", "0.4.9")
  ]
}

// 👱 this is the data model for our registered users in our database
entity User {=psl
  id       Int     @id @default(autoincrement())
  username String  @unique
  password String
psl=}

// ...
```

With this, the Wasp compiler will know what to do and will configure these features for us.

Let’s tell it we want WebSockets, as well. Add the `webSocket` definition to the `main.wasp` file, just between `auth` and `dependencies`:

```jsx
app whereDoWeEat {
	// ... 
  webSocket: {
    fn: import { webSocketFn } from "@server/ws-server.js",
  },
	// ...
}
```

Now we have to define the `webSocketFn`. In the `./src/server` directory create a new file, `ws-server.ts` and copy the following code:

```tsx
import { WebSocketDefinition } from '@wasp/webSocket';
import { User } from '@wasp/entities';

// define the types. this time we will get the entire User object
// in SocketData from the Auth that Wasp automatically sets up for us 🎉
type PollState = {
  question: string;
  options: {
    id: number;
    text: string;
    description: string;
    votes: string[];
  }[];
};
interface ServerToClientEvents {
  updateState: (state: PollState) => void;
}
interface ClientToServerEvents {
  vote: (optionId: number) => void;
  askForStateUpdate: () => void;
}
interface InterServerEvents {}
interface SocketData {
  user: User; 
}

// pass the generic types to the websocketDefinition just like 
// in the previous example
export const webSocketFn: WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents,
  SocketData
> = (io, _context) => {
  const poll: PollState = {
    question: "What are eating for lunch ✨ Let's order",
    options: [
      {
        id: 1,
        text: 'Party Pizza Place',
        description: 'Best pizza in town',
        votes: [],
      },
      {
        id: 2,
        text: 'Best Burger Joint',
        description: 'Best burger in town',
        votes: [],
      },
      {
        id: 3,
        text: 'Sus Sushi Place',
        description: 'Best sushi in town',
        votes: [],
      },
    ],
  };
  io.on('connection', (socket) => {
    if (!socket.data.user) {
      console.log('Socket connected without user');
      return;
    }

    console.log('Socket connected: ', socket.data.user?.username);
    socket.on('askForStateUpdate', () => {
      socket.emit('updateState', poll);
    });

    socket.on('vote', (optionId) => {
      // If user has already voted, remove their vote.
      poll.options.forEach((option) => {
        option.votes = option.votes.filter((username) => username !== socket.data.user.username);
      });
      // And then add their vote to the new option.
      const option = poll.options.find((o) => o.id === optionId);
      if (!option) {
        return;
      }
      option.votes.push(socket.data.user.username);
      io.emit('updateState', poll);
    });

    socket.on('disconnect', () => {
      console.log('Socket disconnected: ', socket.data.user?.username);
    });
  });
};
```

You may have noticed that there’s a lot less configuration and boilerplate needed here in the Wasp implementation. That’s because the: 

- endpoints,
- authentication,
- and Express and [Socket.IO](http://Socket.IO) middleware

are all being handled for you by Wasp. Noice!

<ImgWithCaption
  source="img/websockets-app/Untitled 4.png"
  width="550px"
/>

Let’s go ahead now and run the app to see what we have at this point. 

First, we need to initialize the database so that our Auth works correctly. This is something we didn’t do in the previous example due to high complexity, but is easy to do with Wasp:

```bash
wasp db migrate-dev
```

Once that’s finished, run the app (it my take a while on first run to install all depenedencies):

```bash
wasp start
```

You should see a login screen this time. Go ahead and first register a user, then login:

<ImgWithCaption
  source="img/websockets-app/Untitled 5.png"
  width="550px"
/>

Once logged in, you’ll see the same hardcoded poll data as in the previous example, because, again, we haven’t set up the [Socket.IO](http://Socket.IO) client on the frontend. But this time it should be much easier.

Why? Well, besides less configuration, another nice benefit of working with [TypeScript with Wasp](/docs/advanced/web-sockets), is that you just have to define payload types with matching event names on the server, and those types will get exposed automatically on the client! 

Let’s take a look at how that works now.

In `.src/client/MainPage.tsx`, replace the contents with the following code:

```tsx
import { useState, useMemo, useEffect } from "react";
import { Button, Card } from "flowbite-react";
// Wasp provides us with pre-configured hooks and types based on
// our server code. No need to set it up ourselves!
import {
  useSocketListener,
  useSocket,
  ServerToClientPayload,
} from "@wasp/webSocket";
import useAuth from "@wasp/auth/useAuth";

const MainPage = () => {
	// we can easily access the logged in user with this hook
	// that wasp provides for us
  const { data: user } = useAuth();
  const [poll, setPoll] = useState<ServerToClientPayload<"updateState"> | null>(
    null
  );
  const totalVotes = useMemo(() => {
    return (
      poll?.options.reduce((acc, option) => acc + option.votes.length, 0) ?? 0
    );
  }, [poll]);
	
	// pre-built hooks, configured for us by Wasp
  const { socket } = useSocket(); 
  useSocketListener("updateState", (newState) => {
    setPoll(newState);
  });

  useEffect(() => {
    socket.emit("askForStateUpdate");
  }, []);

  function handleVote(optionId: number) {
    socket.emit("vote", optionId);
  }

  return (
    <div className="w-full max-w-2xl mx-auto p-8">
      <h1 className="text-2xl font-bold">{poll?.question ?? "Loading..."}</h1>
      {poll && (
        <p className="leading-relaxed text-gray-500">
          Cast your vote for one of the options.
        </p>
      )}
      {poll && (
        <div className="mt-4 flex flex-col gap-4">
          {poll.options.map((option) => (
            <Card key={option.id} className="relative transition-all duration-300 min-h-[130px]">
              <div className="z-10">
                <div className="mb-2">
                  <h2 className="text-xl font-semibold">{option.text}</h2>
                  <p className="text-gray-700">{option.description}</p>
                </div>
                <div className="absolute bottom-5 right-5">
                  {user && !option.votes.includes(user.username) ? (
                    <Button onClick={() => handleVote(option.id)}>Vote</Button>
                  ) : (
                    <Button disabled>Voted</Button>
                  )}
                  {!user}
                </div>
                {option.votes.length > 0 && (
                  <div className="mt-2 flex gap-2 flex-wrap max-w-[75%]">
                    {option.votes.map((vote) => (
                      <div
                        key={vote}
                        className="py-1 px-3 bg-gray-100 rounded-lg flex items-center justify-center shadow text-sm"
                      >
                        <div className="w-2 h-2 bg-green-500 rounded-full mr-2"></div>
                        <div className="text-gray-700">{vote}</div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
              <div className="absolute top-5 right-5 p-2 text-sm font-semibold bg-gray-100 rounded-lg z-10">
                {option.votes.length} / {totalVotes}
              </div>
              <div
                className="absolute inset-0 bg-gradient-to-r from-yellow-400 to-orange-500 opacity-75 rounded-lg transition-all duration-300"
                style={{
                  width: `${
                    totalVotes > 0
                      ? (option.votes.length / totalVotes) * 100
                      : 0
                  }%`,
                }}
              ></div>
            </Card>
          ))}
        </div>
      )}
    </div>
  );
};
export default MainPage;
```

In comparison to the previous implementation, Wasp saved us from having to configure the [Socket.IO](http://Socket.IO) client, as well as building our own hooks.

Also, hover over the variables in your client-side code, and you’ll see that the types are being automatically inferred for you! 

Here’s just one example, but it should work for them all:

<ImgWithCaption
  source="img/websockets-app/Untitled 6.png"
  width="550px"
/>

Now if you open up a new private/incognito tab, register a new user, and login, you’ll see a fully working, real-time voting app. The best part is, in comparison to the previous approach, we can log out and back in, and our voting data persists, which is exactly what we’d expect from a production grade app. 🎩

<ImgWithCaption
  source="img/websockets-app/Untitled 1.gif"
  width="550px"
/>

Awesome… 😏

## Comparing the Two Approaches

Now, just because one approach seems easier, doesn’t always mean it’s always better. Let’s give a quick run-down of the advantages and disadvantages of both the implementations above.

|  | Without Wasp | With Wasp |
| --- | --- | --- |
| 😎 Intended User  | Senior Developers, web development teams | Full-stack developers, “Indiehackers”, junior devs |
| 📈 Complexity of Code | Medium-to-High | Low |
| 🚤 Speed | Slower, more methodical | Faster, more integrated |
| 🧑‍💻 Libraries | Any | Socket.IO |
| ⛑ Type safety  | Implement on both server and client | Implement once on server, inferred by Wasp on client |
| 🎮 Amount of control | High, as you determine the implementation | Opinionated, as Wasp decides the basic implementation |
| 🐛 Learning Curve | Complex: full knowledge of front and backend technologies, including WebSockets | Intermediate: Knowledge of full-stack fundamentals necessary. |

### Implementing WebSockets Using React, Express.js (Without Wasp)

Advantages:

1. Control & **Flexibility**: You can approach the implementation of WebSockets in the way that best suits your project's needs, as well as your choice between a [number of different WebSocket libraries](https://www.atatus.com/blog/websocket-libraries-for-nodejs/), not just Socket.IO.

Disadvantages:

1. **More Code & Complexity**: Without the abstractions provided by a framework like Wasp, you might need to write more code and create your own abstractions to handle common tasks. Not to mention the proper configuration of a NodeJS/ExpressJS server (the one provided in the example is very basic)
2. Manual **Type Safety: If you’re working with TypeScript, you have to be more careful typing your  event handlers and payload types coming into and going out from the server, or implement a more type-safe approach yourself.**

### Implementing WebSockets with Wasp (uses React, ExpressJS, and [Socket.IO](http://Socket.IO) under the hood)

Advantages:

1. Fully-Integrated**/Less code**: Wasp provides useful abstractions such as `useSocket` and `useSocketListener` hooks for use in React components (on top of other features like Auth, Async Jobs, Email-sending, DB management, and Deployment), simplifying the client-side code, and allowing for full integration with less configuration.
2. **Type Safety**: Wasp facilitates full-stack type safety for WebSocket events and payloads. This reduces the likelihood of runtime errors due to mismatched data types and saves you from writing even more boilerplate.

Disadvantages:

1. **Learning curve**: Developers unfamiliar with Wasp will need to learn the framework to effectively use it.
2. **Less control**: While Wasp provides a lot of conveniences, it abstracts away some of the details, giving developers slightly less control over certain aspects of socket management.

## Conclusion

In general, how you add WebSockets to your React app depends on the specifics of your project, your comfort level with the available tools, and the trade-offs you're willing to make between ease of use, control, and complexity.

Don’t forget, if you want to check out the full finished code from our “Lunch Voting” example full-stack app, go here: [https://github.com/vincanger/websockets-wasp](https://github.com/vincanger/websockets-wasp)

And if you know of a better, cooler, sleeker way of implementing WebSockets into your apps, let us know in the comments below

<ImgWithCaption
  source="img/websockets-app/Untitled 7.png"
  width="550px"
/>
