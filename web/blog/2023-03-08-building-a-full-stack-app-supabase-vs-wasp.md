---
title: "Building a full-stack app: Supabase vs. Wasp"
authors: [miho]
image: /img/building-a-full-stack-app-supabase-vs-wasp.jpg
tags: [Full-stack, Supabase, Wasp, WebDev]
---

import Link from '@docusaurus/Link';
import ImgWithCaption from './components/ImgWithCaption'

<br/>

<ImgWithCaption
    alt="wasp vs. supabase"
    source="img/building-a-full-stack-app-supabase-vs-wasp.jpg"
/>

## Intro

### What to expect

In this blog post, I will explain how I created the [Phrase Tutor](https://phrasetutor.com/) app for learning Italian phrases using two different technologies. I will share some code snippets to show what was required to build the app with both Wasp and Supabase.

<ImgWithCaption
    alt="Phrase Tutor’s front-end"
    source="img/building-a-full-stack-app-supabase-vs-wasp/phrase_tutor.png"
    caption="Phrase Tutor’s front-end"
/>

As a senior full-stack developer with experience in building many side-projects, I prefer a quick development cycle. I enjoy turning ideas into POCs in just a few days or even hours.

We will examine how each technology can help when building a full-stack app and where Wasp and Supabase excel.

<!--truncate-->

### I wanted to learn Italian fast

Whenever I travel abroad, I enjoy imagining what it would be like to live in that place. For instance, I usually don't like taking crowded public transportation, but for some reason, it brings me joy when I do it in a foreign country. It's all about the feeling that I'm living there. One of the most important things for me to fully experience the culture is to learn the language or, at the very least, be able to not speak English all the time.

<ImgWithCaption
    alt="Pretending to be Italian"
    source="img/building-a-full-stack-app-supabase-vs-wasp/italian.gif"
    caption="Pretending to be Italian"
/>

My girlfriend and I were planning a trip to Italy, and I wanted to learn some Italian. I thought about what would be the easiest way to learn as much as possible with the least amount of effort. I decided that learning the top 100 Italian phrases would be a good start. I had a week to do it, and learning 100 phrases seemed doable if I practiced every day.

### The learning method

In high school, I had a system for learning historical facts and dates quickly called "focusing on things you don’t know".

Here's how it works:

1. Gather a pool of facts you want to learn (e.g. "When did WWI start?" - "1914").
2. Ask yourself each question in the pool.
3. If you know the answer, remove the fact from the pool.
4. If you don't know the answer, keep it in the pool.
5. Repeat with the smaller pool until there are no more facts left.

I made a small app for this and shared it with my classmates, but it didn't go further than that.

Now, I want to use the same method to learn Italian phrases for my trip. So, as a better developer now, I'll make a proper app and host it somewhere 🙂

## Building the Phrase Tutor app

We will create an app that follows the method described above. The app will show you a phrase and you can tell it if you know the translation or not by selecting "I knew it" or "I didn't know it".

<ImgWithCaption
    alt="How the learning in the app should work"
    source="img/building-a-full-stack-app-supabase-vs-wasp/phrases.png"
    caption="How the learning in the app should work"
/>

The app will keep track of your answers and suggest which phrases you should learn next 🕵️

I’ve built the app twice: first with Supabase and then with Wasp. Supabase is a well-rounded open-source Backend as a Service (BaaS) product that adds superpowers to your front-end apps. On the other hand, Wasp is an open-source framework for building full-stack apps that helps to keep the boilerplate low. Let’s see how they compare.

### Initial Supabase version

When I made the initial version, I worked heavily with Vue.js, which I used to create the first version of the Phrase Tutor app. I started by collecting some phrases. I searched on Google for "best Italian phrases to learn" and came across an article titled "100 Italian phrases to learn." (After extracting the phrases from the HTML, I found out that there were only 96 phrases, but that was still good enough for me.)

The initial app contained the phrases in a [JSON file](https://github.com/infomiho/phrase-tutor-supabase/blob/master/src/phrases/italian.json) that the front-end loaded. It was completely static, but it worked.

```json
{
    "id": 1,
    "group": "general",
    "translations": {
        "en": "Yes",
        "it": "Si"
    }
}
```

I put it on Cloudflare Pages and it went live.

I showed it to my girlfriend, but she didn't like some of the phrases I used. If only I had a backend with a database to edit the phrases. Then I had an idea: let's add a database with [Supabase](https://supabase.com/).

Supabase is a managed backend solution that provides a lot of free stuff: a PostgreSQL database and social authentication among other things.

<ImgWithCaption
    alt="Phrase Tutor built with Supabase"
    source="img/building-a-full-stack-app-supabase-vs-wasp/supabase.png"
    caption="Phrase Tutor built with Supabase"
/>

I set up the database tables using the Supabase UI which was pretty straightforward.

The table I needed only had a few fields:

```sql
CREATE TABLE phrases (
    id bigint  NOT NULL,
    group character varying  NULL,
    translations_en text  NOT NULL,
    translations_it text  NOT NULL
);
```

Then I had to seed the database with some SQL. Executing SQL statements is easy with the use of Supabase’s UI. You just log in, open the SQL editor and paste in the code:

```sql
INSERT INTO phrases(id,"group",translations_en,translations_it) VALUES (1,'general','Yes','Si');
INSERT INTO phrases(id,"group",translations_en,translations_it) VALUES (2,'general','No','No');
...
```

Integrating Supabase into my existing front-end app was simple using their [Javascript SDK](https://github.com/infomiho/phrase-tutor-supabase/blob/master/src/services/supabase.ts#L4). If you're familiar with Firebase, it should feel similar. Essentially, you build your SQL queries on the frontend and use the resulting data in your app.

Using the SDK felt pretty straightforward and I could get what I wanted out of the database without much hassle.

```jsx
const { data, error } = await supabase.from("phrases").select("*");
```

And just like that, my static Vue.js app had a database to rely on 🎉

Adding the login with Google was a matter of enabling it in Supabase UI and setting up the Client ID and Client Secret variables. In order to trigger the login process with Google, I [once again](https://github.com/infomiho/phrase-tutor-supabase/blob/master/src/stores/user.ts#L9) relied on their Javascript SDK.

```jsx
supabase.auth.signInWithOAuth({ provider: "google" });
```

Awesome! I'm glad that I can now edit the phrases and that there is a login feature that I plan to use later.

In the future, I have plans to add more languages to the app and also allow registered users to contribute new phrases and translations. I believe this will make the app more useful and engaging for language learners.

And just like that, my app went from a pure static app to an app with a database and Google login 🤯

:::info
Check out the deployed app written with Vue.js and Supabase: [https://phrase-tutor.pages.dev](https://phrase-tutor.pages.dev/)
:::

:::info
View the source [here](https://github.com/infomiho/phrase-tutor-supabase)
:::

### Joining Wasp and dogfooding it

Some background before the second part: I started working at Wasp earlier this year. I'm really happy to work on a technology that solves a problem I care about: when I do side-projects, I dislike writing the same dull parts every time from scratch. I copy and paste from my previous side projects, but eventually, the code snippets become old and outdated.

Naturally, I wanted to test out Wasp by rewriting one of my side projects. I decided to see how Wasp could work with the Phrase Tutor project.

Wasp works by having an easy-to-understand config file called `main.wasp` which coordinates your pieces of client and server functionalities. Its main purpose is to keep you productive and focused on writing interesting bits. It feels pretty much like using a web framework that covers your whole app.

<ImgWithCaption
    alt="Phrase Tutor built with Wasp"
    source="img/building-a-full-stack-app-supabase-vs-wasp/wasp.png"
    caption="Phrase Tutor built with Wasp"
/>

Let's begin by creating the data models. Wasp uses Prisma under the hood to communicate with your database, which makes it easy to manage your database without worrying about the details. This is just one of the many choices the framework made for me, and I appreciate the feeling of using a setup that works.

I had to first declare all of the entities I needed with Prisma PSL in the Wasp config file.

```jsx
entity Phrase {=psl
  id Int @id @default(autoincrement())
  group String
  phrase String
  translations Translation[]
psl=}

entity Language {=psl
  id Int @id @default(autoincrement())
  name String @unique
  emoji String
  translations Translation[]
psl=}

entity Translation {=psl
  id Int @id @default(autoincrement())
  phraseId Int
  languageId Int
  translation String
  phrase Phrase @relation(fields: [phraseId], references: [id], onDelete: Cascade)
  language Language @relation(fields: [languageId], references: [id], onDelete: Cascade)
psl=}

```

I'm using a PostgreSQL database again, and you can see that the field definitions are similar.

I improved the data schema a bit by defining three tables instead of one. I separated the concept of a `Phrase` from the concepts of `Language` and `Translation`. This will make it easier to add new languages in the future.

I added some phrases to the database using Prisma and a [Wasp action](https://github.com/infomiho/phrase-tutor-wasp/blob/master/src/server/actions.js#L1):

```tsx
export async function seedItalianPhrases(args, context) {
    const data = [
       {
            id: 1,
            group: "general",
            translations_en: "Yes",
            translations_it: "Si"
        },
        ...
    ]
    for (const phrase of seedPhrases) {
        await context.entities.Phrase.create({
            ...
        });
    }
}
```

Let’s now look at what I needed to do to get the data flowing from the backend to my React app.

First, I declared a query in my Wasp config file:

```c
app phraseTutor {
  ...
}
...

query fetchAllPhrases {
  fn: import { getAllPhrases } from "@server/queries.js",
  entities: [Phrase]
}
```

Then I wrote the code for my backend to fetch the phrases. You’ll notice it’s quite similar to the code I wrote for fetching phrases with the Supabase SDK, but I had to include the `translations` relation since we now have multiple tables.

```jsx
// My query got the Prisma entity through the context parameter
// which I just used to fetch all the phrases
export async function getAllPhrases(args, context) {
    return context.entities.Phrase.findMany({
        include: {
            translations: true
        }
    });
}
```

And lastly, I could just import the query into my React app. It’s set up in a way that it handles cache invalidation automatically, one less thing to worry about, which is awesome 😎

```jsx
// Wasp relies on React Query in the background
const { data: phrases, isLoading } = useQuery(fetchAllPhrases);
```

Let’s also add support for Google auth for our app. It involves declaring you want it in the Wasp file, adding some env variables and using it in the React application.

We declare it to the Wasp file by adding the `google` key under `auth`:

```jsx
app phraseTutor {
  ...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialUser,
    methods: {
      // Define we want the Google auth
      google: {
        // Optionally, we can adjust what is saved from the user's data
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/"
  },
  ...
}

// Some of the entities needed for auth
entity User {=psl
  id Int @id @default(autoincrement())
  username String @unique
  password String
  profilePicture String
  externalAuthAssociations SocialUser[]
  createdAt DateTime @default(now())
psl=}

entity SocialUser {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```

And … that’s it. We can now use the Google auth in our frontend 🎉

```jsx
import { signInUrl as googleSignInUrl } from "@wasp/auth/helpers/Google";
...
const { data: user } = useAuth();
```

Writing a full-stack React and Express.js with Wasp felt like a guided experience; I didn't have to focus too hard on the dev tooling, building, or deploying.

Instead, I could focus on the logic needed for Phrase Tutor to work and just run `wasp start` most of the time. I did need to write some extra code to get everything running, but I'm free to customize this code however I want.

:::info
Check out the deployed project built with Wasp: [https://phrasetutor.com](https://phrasetutor.com/)
:::

:::info
View the source [here](https://github.com/infomiho/phrase-tutor-wasp)
:::

### Let's compare some of the features

I want to compare the features of Supabase and Wasp. It's good to think about different ways to do things and their pros and cons.

| Feature                      | Supabase                                                          | Wasp                                                                            |
| ---------------------------- | ----------------------------------------------------------------- | ------------------------------------------------------------------------------- |
| Getting data from the API    | Use the Supabase JS SDK to query database tables                  | Declare queries, write JS code to execute them and import them in the React app |
| Custom business logic        | Writing custom PostgreSQL procedures or by writing edge functions | Declare actions in the Wasp file and write server-side JS                       |
| Defining the database schema | Visual editor or by CREATE TABLE query                            | Define Prisma schema                                                            |
| Auth                         | Enable in UI                                                      | Enable it in the Wasp file                                                      |
| Deployment                   | Supabase managed instance or self-host it                         | Deploy anywhere, support for https://fly.io one line deployment                  |

With Supabase, I liked how familiar the SDK felt and their UI made it easy to configure parts of my backend. I didn’t need to think about deploying Supabase since I used their hosted version, but it did get paused after 1 week of inactivity.

On the other hand, Wasp felt like the glue for my React + Express.js + Prisma app and I needed to write more code to get things done. It felt more explicit because I wrote code closer to what I would normally write. I deployed it to [fly.io](https://fly.io) with the Wasp command `wasp deploy fly launch` and it’s now live on [https://phrasetutor.com](https://phrasetutor.com/)

## Conclusion

### It's all about the use case

Choosing the right solution for your needs can be difficult. That's why it's important to try out different options and see how they work for you. In this case, I compared two options: Supabase and Wasp.

Supabase is a great choice if you want a well-rounded open-source BaaS product that adds superpowers to your front-end apps. It provides a lot of free stuff, such as a PostgreSQL database and social authentication, which can make development easier and faster. It also has a nice SDK and UI that the end user can use to easily define their app's configuration.

Wasp is an open-source framework for building full-stack apps that helps out with keeping the boilerplate low. It is a bit more explicit about some things, such as defining your auth entities, but that can be a plus when you have more advanced use cases. By using Wasp as the glue for your full-stack application, you can have the best of both worlds: a development and production setup that works out of the box while still allowing you to develop your app any way you like.

In the case of Phrase Tutor, I liked working with both Supabase and Wasp. I did, however, get a different feeling from working with the two technologies. With Supabase I felt like my front-end app got instant superpowers and it now has a database and login, which was nice considering the effort I had to put in. But now I had a black-box dependency that I needed to build around.

When I used Wasp to rebuild Phrase Tutor, it felt different because it was a full-stack app. I had more control over the application code, so I could change it and evolve it as I wanted. I felt like I had built an app that could grow in any direction. Although I had to write more code, it felt like a good trade-off for future needs.

To decide which option is best for you, I would suggest trying both and seeing how you feel. It is easy to set up both tools and see if they make sense for you.

<ImgWithCaption
    alt="Grazie for reading 🙃"
    source="img/building-a-full-stack-app-supabase-vs-wasp/thank_you.png"
    caption="Grazie for reading 🙃"
/>

If you try out the Phrase Tutor app, please let me know what you think. You can reach me on Twitter. I'm always looking for ways to make it better.
