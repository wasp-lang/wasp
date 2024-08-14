---
title: 'How to Add Auth with Lucia to Your React/Next.js App - A Step by Step Guide'
authors: [lucaslima]
image: /img/lua-auth/lucia-auth-banner.png
tags: [webdev, tech, react, nextjs, tutorial]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="How to Add Auth to Your App"
    source="/img/lua-auth/lucia-auth-banner.png"
/>

Although authentication is one of the most common web app features, there are so many different ways to go about it, which makes it a very non-trivial task. In this post, I will share my personal experience using Lucia - a modern, framework-agnostic authentication library that has been getting, deservedly so, a lot of love from the community in recent months.

First, I will demonstrate how you can implement it within your Next.js application through a step-by-step guide you can follow. It will require a fair amount of code and configuration, but the process itself is quite straightforward.

Secondly, we’ll see how to achieve the same with [Wasp](https://wasp-lang.dev/) in just a few lines of code. Wasp is a batteries-included, full-stack framework for React & Node.js that uses Lucia under the hood to implement authentication. It runs fully on your infrastructure and is 100% open-source and free.

![auth with Wasp](/img/lua-auth/comparison.png)

## Why Lucia?

When it comes to adding authentication to your applications, there are several popular solutions available. For instance, [Clerk](https://clerk.com/) offers a paid service, while [NextAuth.js](https://next-auth.js.org/) is an open-source solution alongside [Lucia](https://lucia-auth.com/), which has become quite popular recently. 

These tools provide robust features, but committing to third-party services — which not only adds another layer of complexity but also have paid tiers you have to keep an eye on — might be an overkill for a small project. In-house solutions keep things centralized but leave it to a developer to implement some of the mentioned features.

In our case, Lucia has proved to be a perfect middle ground - it’s not a third-party service and does not require a dedicated infrastructure, but it also provides a very solid foundation that’s easy to build upon.

Now, let’s dive into a step-by-step guide on how to implement your own authentication with Next.js and Lucia.

### Step 1: Setting up Next.js

First, create a new Next.js project:

```bash
npx create-next-app@latest my-nextjs-app
cd my-nextjs-app
npm install
```

### Step 2: Install Lucia

Next, install Lucia:

```bash
npm install lucia
```

### Step 3: Set up Authentication

Create an `auth` file in your project and add the necessary files for Lucia to be imported and initialized. It has a bunch of adapters for different databases, and you can check them all [here](https://lucia-auth.com/database/). In this example, we’re going to use SQLite:

```jsx
// lib/auth.ts
import { Lucia } from "lucia";
import { BetterSqlite3Adapter } from "@lucia-auth/adapter-sqlite";

const adapter = new BetterSQLite3Adapter(db); // your adapter

export const lucia = new Lucia(adapter, {
  sessionCookie: {
    // this sets cookies with super long expiration
    // since Next.js doesn't allow Lucia to extend cookie expiration when rendering pages
    expires: false,
    attributes: {
      // set to `true` when using HTTPS
      secure: process.env.NODE_ENV === "production"
    }
  }
});

// To get some good Typescript support, add this!
declare module "lucia" {
  interface Register {
    Lucia: typeof lucia;
  }
}
```

### Step 4: Add User to DB

Let’s add a database file to contain our schemas for now:

```tsx title="lib/db.ts"
import sqlite from "better-sqlite3";

export const db = sqlite("main.db");

db.exec(`CREATE TABLE IF NOT EXISTS user (
    id TEXT NOT NULL PRIMARY KEY,
    github_id INTEGER UNIQUE,
    username TEXT NOT NULL
)`);

db.exec(`CREATE TABLE IF NOT EXISTS session (
    id TEXT NOT NULL PRIMARY KEY,
    expires_at INTEGER NOT NULL,
    user_id TEXT NOT NULL,
    FOREIGN KEY (user_id) REFERENCES user(id)
)`);

export interface DatabaseUser {
  id: string;
  username: string;
  github_id: number;
}
```

### Step 5: Implement Login and Signup

To make this happen, we firstly have to create a GitHub OAuth app. This is relatively simple, you create it, add the necessary ENVs and callback URLs into your application and you’re good to go. You can [follow GitHub docs](https://docs.github.com/en/apps/oauth-apps/building-oauth-apps/creating-an-oauth-app) to check how to do that.

```tsx title=".env.local"
GITHUB_CLIENT_ID=your-github-client-id
GITHUB_CLIENT_SECRET=your-github-client-secret
```

After that, it’s a matter of adding login and signup functionalities to your pages, so, let’s do that real quick: 

```tsx title="login/page.tsx"
import { validateRequest } from "@/lib/auth";
import { redirect } from "next/navigation";

export default async function Page() {
  const { user } = await validateRequest();
  if (user) {
    return redirect("/");
  }
  return (
    <>
      <h1>Sign in</h1>
      <a href="/login/github">Sign in with GitHub</a>
    </>
  );
}
```

After adding the page, we also have to add the login redirect to GitHub and the callback that’s going to be called. Let’s first add the login redirect with the authorization URL:

```tsx title="login/github/route.ts"
import { generateState } from "arctic";
import { github } from "../../../lib/auth";
import { cookies } from "next/headers";

export async function GET(): Promise<Response> {
  const state = generateState();
  const url = await github.createAuthorizationURL(state);

  cookies().set("github_oauth_state", state, {
    path: "/",
    secure: process.env.NODE_ENV === "production",
    httpOnly: true,
    maxAge: 60 * 10,
    sameSite: "lax"
  });

  return Response.redirect(url);
}
```

 And finally, the callback (which is what we actually add in GitHub OAuth):

```tsx title="login/github/callback/route.ts"
import { github, lucia } from "@/lib/auth";
import { db } from "@/lib/db";
import { cookies } from "next/headers";
import { OAuth2RequestError } from "arctic";
import { generateId } from "lucia";

import type { DatabaseUser } from "@/lib/db";

export async function GET(request: Request): Promise<Response> {
  const url = new URL(request.url);
  const code = url.searchParams.get("code");
  const state = url.searchParams.get("state");
  const storedState = cookies().get("github_oauth_state")?.value ?? null;
  if (!code || !state || !storedState || state !== storedState) {
    return new Response(null, {
      status: 400
    });
  }

  try {
    const tokens = await github.validateAuthorizationCode(code);
    const githubUserResponse = await fetch("https://api.github.com/user", {
      headers: {
        Authorization: `Bearer ${tokens.accessToken}`
      }
    });
    const githubUser: GitHubUser = await githubUserResponse.json();
    const existingUser = db.prepare("SELECT * FROM user WHERE github_id = ?").get(githubUser.id) as
      | DatabaseUser
      | undefined;

    if (existingUser) {
      const session = await lucia.createSession(existingUser.id, {});
      const sessionCookie = lucia.createSessionCookie(session.id);
      cookies().set(sessionCookie.name, sessionCookie.value, sessionCookie.attributes);
      return new Response(null, {
        status: 302,
        headers: {
          Location: "/"
        }
      });
    }

    const userId = generateId(15);
    db.prepare("INSERT INTO user (id, github_id, username) VALUES (?, ?, ?)").run(
      userId,
      githubUser.id,
      githubUser.login
    );
    const session = await lucia.createSession(userId, {});
    const sessionCookie = lucia.createSessionCookie(session.id);
    cookies().set(sessionCookie.name, sessionCookie.value, sessionCookie.attributes);
    return new Response(null, {
      status: 302,
      headers: {
        Location: "/"
      }
    });
  } catch (e) {
    if (e instanceof OAuth2RequestError && e.message === "bad_verification_code") {
      // invalid code
      return new Response(null, {
        status: 400
      });
    }
    return new Response(null, {
      status: 500
    });
  }
}

interface GitHubUser {
  id: string;
  login: string;
}
```

Other important thing here is that, now, we’re going with GitHub OAuth, but, generally, these libraries contain a bunch of different login providers (including simple username and password), so it’s usually just a pick and choose if you want to add other providers. 

```tsx title="lib/auth.ts"
import { Lucia } from "lucia";
import { BetterSqlite3Adapter } from "@lucia-auth/adapter-sqlite";
import { db } from "./db";
import { cookies } from "next/headers";
import { cache } from "react";
import { GitHub } from "arctic";

import type { Session, User } from "lucia";
import type { DatabaseUser } from "./db";

// these two lines here might be important if you have node.js 18 or lower. 
// you can check Lucia's documentation in more detail if that's the case 
// (https://lucia-auth.com/getting-started/nextjs-app#polyfill)
// import { webcrypto } from "crypto";
// globalThis.crypto = webcrypto as Crypto;

const adapter = new BetterSqlite3Adapter(db, {
  user: "user",
  session: "session"
});

export const lucia = new Lucia(adapter, {
  sessionCookie: {
    attributes: {
      secure: process.env.NODE_ENV === "production"
    }
  },
  getUserAttributes: (attributes) => {
    return {
      githubId: attributes.github_id,
      username: attributes.username
    };
  }
});

declare module "lucia" {
  interface Register {
    Lucia: typeof lucia;
    DatabaseUserAttributes: Omit<DatabaseUser, "id">;
  }
}

export const validateRequest = cache(
  async (): Promise<{ user: User; session: Session } | { user: null; session: null }> => {
    const sessionId = cookies().get(lucia.sessionCookieName)?.value ?? null;
    if (!sessionId) {
      return {
        user: null,
        session: null
      };
    }

    const result = await lucia.validateSession(sessionId);
    // next.js throws when you attempt to set cookie when rendering page
    try {
      if (result.session && result.session.fresh) {
        const sessionCookie = lucia.createSessionCookie(result.session.id);
        cookies().set(sessionCookie.name, sessionCookie.value, sessionCookie.attributes);
      }
      if (!result.session) {
        const sessionCookie = lucia.createBlankSessionCookie();
        cookies().set(sessionCookie.name, sessionCookie.value, sessionCookie.attributes);
      }
    } catch {}
    return result;
  }
);

export const github = new GitHub(process.env.GITHUB_CLIENT_ID!, process.env.GITHUB_CLIENT_SECRET!);
```

### Step 6: Protect Routes

After adding all that stuff to make the login properly work, we just have to ensure that routes are protected by checking authentication status — in this case, this is a simple page that shows username, id and a button in case signed in, and redirects to /login, where the user will complete the login above through a form.

```tsx title="profile/page.tsx"
import { lucia, validateRequest } from "@/lib/auth";
import { redirect } from "next/navigation";
import { cookies } from "next/headers";

export default async function Page() {
  const { user } = await validateRequest();
  if (!user) {
    return redirect("/login");
  }
  return (
    <>
      <h1>Hi, {user.username}!</h1>
      <p>Your user ID is {user.id}.</p>
      <form action={logout}>
        <button>Sign out</button>
      </form>
    </>
  );
}

async function logout(): Promise<ActionResult> {
  "use server";
  const { session } = await validateRequest();
  if (!session) {
    return {
      error: "Unauthorized"
    };
  }

  await lucia.invalidateSession(session.id);

  const sessionCookie = lucia.createBlankSessionCookie();
  cookies().set(sessionCookie.name, sessionCookie.value, sessionCookie.attributes);
  return redirect("/login");
}

interface ActionResult {
  error: string | null;
}
```

Piece of cake, isn’t it? Well, not really. 

Let’s recap which steps were necessary to actually make this happen:

- Set up your app.
- Add Lucia.
- Set up authentication.
- Add User to DB.
- Obtain GitHub OAuth credentials and configure your environment variables.
- Create some util functions.
- Add Login and Sign up routes, with custom made components.
- Finally, create a protected route.

![https://media2.giphy.com/media/3ofSBnYbEPePeigIMg/giphy.gif?cid=7941fdc6x77sivlvr6hs2yu5aztvwjvhgugv6b718mjanr2h&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media2.giphy.com/media/3ofSBnYbEPePeigIMg/giphy.gif?cid=7941fdc6x77sivlvr6hs2yu5aztvwjvhgugv6b718mjanr2h&ep=v1_gifs_search&rid=giphy.gif&ct=g)

Honestly, when trying to create something cool **FAST**, repeating these steps and debugging a few logical problems here and there that always occur can feel a little bit frustrating. Soon, we’ll take a look at Wasp’s approach to solving that same problem and we’ll be able to compare how much easier Wasp’s auth implementation process is.

In case you want to check the whole code for this part, [Lucia has an example repo](https://github.com/lucia-auth/examples/tree/main/nextjs-app/github-oauth) (that is the source of most of the code shown), so, you can check it out if you’d like.

## Wasp Implementation

Now, let’s go through how we can achieve the same things with Wasp 🐝. Although it still uses Lucia in the background, Wasp takes care of all the heavy-lifting for you, making the process much quicker and simpler. Let’s check out the developer experience for ourselves.

Before we just into it, in case you’re more of a visual learner, here’s a 1-minute video showcasing auth with wasp.

<div className='video-container'>
    <iframe src="https://www.youtube.com/embed/Qiro77q-ulI?si=JVBcFAk5dnR3Q0PL" frameborder="1" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
</div>

As seen in the video, Wasp is a framework for building apps with the benefits of using a configuration file to make development easier. It handles many repetitive tasks, allowing you to focus on creating unique features. In this tutorial, we’ll also learn more about the Wasp config file and see how it makes setting up authentication simpler.

### Step 1: Create a Wasp Project

```bash
curl -sSL https://get.wasp-lang.dev/installer.sh | sh
wasp new my-wasp-app
cd my-wasp-app
```

### Step 2: Add the User entity into our DB

As simple as defining the `app.auth.userEntity` entity in the `schema.prisma` file and running some migrations:

```prisma
model User {
  id Int @id @default(autoincrement())
  email   String   @unique
  name    String?
  // Add your own fields below
  // ...
}
```

### Step 3: Define Authentication

In your main Wasp configuration, add the authentication provider you want for your app

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.14.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      // 2. Enable Github Auth
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

And after that, just run in your terminal:

```bash
wasp db migrate-dev
```

### Step 4: Get your GitHub OAuth credentials and app running

This part is similar for both frameworks, you can follow the documentation GitHub provides here to do so: [Creating an OAuth app - GitHub Docs](https://docs.github.com/en/apps/oauth-apps/building-oauth-apps/creating-an-oauth-app). For wasp app, the callback urls are: 

- While developing: `http://localhost:3001/auth/github/callback`
- After deploying: `https://your-server-url.com/auth/github/callback`

After that, get your secrets and add it to the env file:

```tsx title=".env.server"
GITHUB_CLIENT_ID=your-github-client-id
GITHUB_CLIENT_SECRET=your-github-client-secret
```

### Step 5: Add the routes and pages

Now, let’s simply add some routing and the page necessary for login — the process is way easier since Wasp has pre-built Login and Signup Forms, we can simply add those directly:

```wasp title="main.wasp"
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@src/SignupPage"
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@src/LoginPage"
}
```

```tsx title="src/LoginPage.jsx"
import { Link } from 'react-router-dom'
import { LoginForm } from 'wasp/client/auth'

export const LoginPage = () => {
  return (
    <div style={{ maxWidth: '400px', margin: '0 auto' }}>
      <LoginForm />
      <br />
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </div>
  )
}
```

```tsx title="src/SignupPage.jsx"
import { Link } from 'react-router-dom'
import { SignupForm } from 'wasp/client/auth'

export const SignupPage = () => {
  return (
    <div style={{ maxWidth: '400px', margin: '0 auto' }}>
      <SignupForm />
      <br />
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </div>
  )
}
```

And finally, for protecting routes, is as simple as changing it in `main.wasp` adding **`authRequired: true`** , so, we can simply add it like this:

```wasp title="main.wasp"
page MainPage {
  component: import Main from "@src/pages/Main",
  authRequired: true
}
```

If you’d like to check this example in more depth, feel free to check this repo here: [wasp/examples/todo-typescript at release · wasp-lang/wasp (github.com)](https://github.com/wasp-lang/wasp/tree/release/examples/todo-typescript).
Other great place to check is their documentation, which can be found [here](https://wasp-lang.dev/docs/auth/overview). It covers most of what I said here, and even more (e.g. the awesome new [hooks](https://wasp-lang.dev/docs/auth/auth-hooks) that came with Wasp v0.14)

![https://media4.giphy.com/media/nDSlfqf0gn5g4/giphy.gif?cid=7941fdc6oxsddr7p8rjsuavcyq7ugiad8iqdu1ei25urcge4&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media4.giphy.com/media/nDSlfqf0gn5g4/giphy.gif?cid=7941fdc6oxsddr7p8rjsuavcyq7ugiad8iqdu1ei25urcge4&ep=v1_gifs_search&rid=giphy.gif&ct=g)

Way easier, isn’t it? Let’s review the steps we took to get here:

- Set up the project.
- Add the User entity to the database.
- Define authentication in the main Wasp configuration.
- Obtain GitHub OAuth credentials and configure your environment variables.
- Add routes and pages for login and signup with pre-built, easy-to-use components.
- Protect routes by specifying `authRequired` in your configuration.

### Customizing Wasp Auth

If you need more control and customization over the authentication flow, Wasp provides Auth hooks that allow you to tailor the experience to your app's specific needs. These hooks enable you to execute custom code during various stages of the authentication process, ensuring that you can implement any required custom behavior.

For more detailed information on using Auth hooks with Wasp, visit the [Wasp documentation](https://wasp-lang.dev/docs/auth/auth-hooks).

### Bonus Section: Adding Email/Password Login with Wasp and Customizing Auth

Now let’s imagine we want to add email and password authentication — with all the usual features we’d expect that would follow this login method (e.g. reset password, email verification, etc.). 

With Wasp, all we have to do is add a few lines to your main.wasp file, so, simply updating your Wasp configuration to include email/password authentication makes it work straight out of the box!

![https://wasp-lang.dev/img/auth-ui/auth-demo-compiler.gif](https://wasp-lang.dev/img/auth-ui/auth-demo-compiler.gif)

Wasp will handle the rest, also updating UI components and ensuring a smooth and secure authentication flow.

```wasp title="main.wasp"
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

Implementing this in Next.js with Lucia would take a lot more work, involving a bunch of different stuff from actually sending the emails, to generating the verification tokens and more. They reference this [here](https://lucia-auth.com/guides/email-and-password/email-verification-links), but again, Wasp’s Auth makes the whole process way easier, handling a bunch of the complexity for us while also giving a bunch of other UI components, ready to use, to ease the UI details (e.g. `VerifyEmailForm`, `ForgotPasswordForm` and, `ResetPasswordForm`).

The whole point here is the difference in time and developer experience in order to implement the same scenarios. For the Next.js project with Lucia, you will spend at least a few hours implementing everything if you’re going all by yourself. That same experience translates to no more than 1 hour with Wasp. What to do with the rest of the time? **Implement the important stuff your particular business requires!**


## Can you show us your support?

![https://media2.giphy.com/media/l0MYAs5E2oIDCq9So/giphy.gif?cid=7941fdc6l6i66eq1dc7i5rz05nkl4mgjltyv206syb0o304g&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media2.giphy.com/media/l0MYAs5E2oIDCq9So/giphy.gif?cid=7941fdc6l6i66eq1dc7i5rz05nkl4mgjltyv206syb0o304g&ep=v1_gifs_search&rid=giphy.gif&ct=g)

Are you interested in more content like this? Sign up for [our newsletter](https://wasp-lang.dev/#signup) and give us [a star on GitHub](https://www.github.com/wasp-lang/wasp)! We need your support to keep pushing our projects forward 😀

### Conclusion

![https://media2.giphy.com/media/l1AsKaVNyNXHKUkUw/giphy.gif?cid=7941fdc6u6vp4j2gpjfuizupxlvfdzskl03ncci2e7jq17zr&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media2.giphy.com/media/l1AsKaVNyNXHKUkUw/giphy.gif?cid=7941fdc6u6vp4j2gpjfuizupxlvfdzskl03ncci2e7jq17zr&ep=v1_gifs_search&rid=giphy.gif&ct=g)

I think that if you’re a developer who wants to get things done, you probably noted the significant difference in complexity levels of both of those implementations. 

By reducing boilerplate and abstracting repetitive tasks, Wasp allows developers to focus more on building unique features rather than getting bogged down by authentication details. This can be especially beneficial for small teams or individual developers aiming to launch products quickly.

Of course, generally when we talk abstractions, it always comes with the downside of losing the finesse of a more personal implementation. In this case, Wasp provides a bunch of stuff for you to implement around and uses Lucia on the background, so the scenario where there’s a mismatch of content implementation is highly unlikable to happen.

In summary, while implementing your own authentication with Next.js and Lucia provides complete control and customization, it can be complex and time-consuming. On the other hand, using a solution like Wasp simplifies the process, reduces code length, and speeds up development. 