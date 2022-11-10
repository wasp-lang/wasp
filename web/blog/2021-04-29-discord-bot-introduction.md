---
title: How to implement a Discord bot (in NodeJS) that requires new members to introduce themselves
authors: [martinsos]
tags: [discord, nodejs]
---

import useBaseUrl from '@docusaurus/useBaseUrl';

<p align="center">
  <figure>
    <img alt="Guest introducing themselves and getting full-access."
        src={useBaseUrl('img/discord-introduction-example.png')}
    />
    <figcaption>A Guest user getting access by introducing themselves in the "introductions" channel.</figcaption>
  </figure>
</p>

At [Wasp](https://wasp-lang.dev), we have a Discord server for our community, where we talk with people interested in and using Wasp - Waspeteers!

In the beginning, we knew everybody in the community by their name, but as it started growing, we had a lot of people joining that never wrote anything, and the community started feeling less homey, less intimate.

This was when we decided to make it required for the new members to introduce themselves to gain access to the community.
We knew that with this kind of barrier we would probably lose some potential new Waspeteers, but those that would go through it would be more engaged and better integrated.

We found no other way to accomplish this automatically but to implement our own Discord bot.
In this post I will describe in detail how we did it.

<!--truncate-->

## High-level approach

We want the following: when a new user comes to our Discord server, they should be able to access only "public" channels, like `rules`, `contributing`, and most importantly, `introductions`, where they could introduce themselves.

Once they introduced themselves in the `introductions` channel, they would get access to the rest of the channels.

<p align="center">
  <figure>
    <img alt="Channels user can see when Guest vs when full member."
        src={useBaseUrl('img/wasp-guest-vs-waspeteer.png')}
        height="400px"
    />
    <figcaption>Left: what Guest sees; Right: what Waspeteer sees.</figcaption>
  </figure>
</p>


In Discord, access control is performed via roles. There are two ways to accomplish what we need:
1. **Adding a role that grants access**. When they join, they have no roles. Once they introduce themselves, they are granted a role (e.g. `Member` or `Waspeteer`) that is required to access the rest of the server.
2. **Removing a role that forbids access**. When they join, they are automatically assigned a role `Guest`, for which we configured the non-public channels to deny access. Once they introduce themselves, the role `Guest` gets removed and they gain access to the rest of the server.

We decided to go with the second approach since it means we don't have to assign all the existing members with a new role. From now on, we will be talking about how to get this second approach working.

To get this going, we need to do the following:
1. Create role `Guest`.
2. Ensure that the `Guest` role has permissions to access only "public" channels.
   One convenient way to go about this is to disable "View Channels" permission for the role `Guest` at the level of Category, so it propagates to all the channels in it, instead of doing it for every single channel.
   Once you are done, use the "See server as a role" feature of Discord to confirm that you set the permissions as you wanted.
3. Automatically inform new members that they must introduce themselves in the `introductions` channel with `!intro <text_about_me>` to gain access to the rest of the server.
   This can be done via Discord's "Welcome Screen" feature or via one of the many existing Discord bots out there offering this functionality.
4. Automatically assign the `Guest` role to a new member when they join the server.
5. Automatically remove the `Guest` role when a member introduces themselves in the public `introductions` channel.

#1, #2 and #3 are relatively straight-forward.

For the #4 (automatic assignment of the role when a new member joins the server), since Discord doesn't support this directly, you will need a bot to do it.
Luckily, many bots allow you to auto-assign roles when new members join, and I ended up using [MEE6](https://mee6.xyz/) for this.
I set it up so that when a new member joins, they are immediately assigned a `Guest` role.

However, for the #5 (remove or assign the role on a message in a specific channel), the situation is more complicated - I couldn't find a single bot out there that supports this!
The closest I got was with [Carl Bot](https://carl.gg/) and its "tags" feature, which allows you to write custom code, but in the end, it turned out to be too restrictive to accomplish this.
Therefore, I ended up implementing our own bot (Wasp Bot) that does this.

## Implementing a Discord Bot (NodeJS)

I decided to implement a bot in NodeJS since it is easy to get started quickly and there is a good Discord library.

I will describe how to create it step by step below, but [here is the final code of the bot](https://github.com/wasp-lang/wasp-bot/tree/4b3858202622c7635aeb6f1d71d9ba9781eea6eb) if you want to skip ahead.

### Defining bot on Discord and adding it to your server.

Before we even start implementing the bot, we will tell Discord about it first, in order to obtain the neccessary credentials that we will use in our code, and we will add the bot to our server.
There are many tutorials already on how to do this, so I will keep it short.

1. Go to Discord Developer Portal, create a new Application -> I named it `Wasp`.
2. Go to the "Bot" part of Application "Settings" and add a new bot. I named it `WaspBot`.
3. On the "Bot" page of your freshly created bot, there is a "TOKEN" part -> create a mental note about it, we will need this later when running our bot.
4. Go to the "OAuth2" part of Application "Settings". Here we will define which permissions our bot will have.
   This is done by checking the permissions we want to give it and then following the URL that will be generated based on our choices.
   - Check the `bot` under the "SCOPES" section.
   - Scroll down further to find the "BOT PERMISSIONS" section. There, check the `Manage Roles`, `View Channels`, `Read Message History`, and `Send Messages`.
   - Under the "SCOPES" section above, you will see a URL. Copy it into the browser and follow the steps to add the bot to your server.

### Creating a basic bot in NodeJS

In a directory where your code will be, create a new npm project with `npm init` -> set the entry point to `bot.js` instead of `index.js`. This will result in a `package.json` file being generated.

We will need one important dependency, `discord.js`, to make it easy to work with Discord's API.
Add it with `npm install -S discord.js`.

Now, create `bot.js` file next to `package.json` with following content:
```js title="bot.js"
const Discord = require('discord.js')

const BOT_TOKEN = process.env.BOT_TOKEN

const bot = new Discord.Client()
bot.login(BOT_TOKEN)

bot.on('ready', function (evt) {
  console.log(`Logged in as: ${bot.user.tag}.`)
})
```

This is it! Run
```
DISCORD_BOT=<TOKEN_OF_YOUR_DISCORD_BOT> node bot.js
```
and you should see output about successful login, in my case it was `Logged in as: WaspBot#1234`.

### Detecting a valid introduction from a member

**NOTE**: For the following part, I was using [Discord.js](https://discord.js.org/#/) docs to figure out how to do it, so if you need more details on a specific step, check them out.

Now is the moment to define exactly how we want the introduction process to go.
So, let's say that the correct way for new members to introduce themselves is by sending a message to the `introductions` channel that starts with `!intro ` and follows with at least 20 characters of text (to ensure the introduction is not too short).
`!intro` makes it easy for our bot to know when to act (in Discord, bot commands often start with `!<something>`).

Let's add the needed code to `bot.js`:
```js title="bot.js"
...

const INTRODUCTIONS_CHANNEL_ID = "<YOU_WILL_HAVE_TO_FIND_THIS_ON_DISCORD_SERVER>"

bot.on('message', async msg => {
  if (msg.content.startsWith('!intro ')) {
    if (msg.channel.id.toString() !== INTRODUCTIONS_CHANNEL_ID) {
      const introductionsChannelName =
        msg.guild.channels.resolve(INTRODUCTIONS_CHANNEL_ID).name
      return msg.reply(
        `Please use !intro command in the ${introductionsChannelName} channel!`
      )
    }

    const introMsg = msg.content.substring('!intro '.length).trim()
    const minMsgLength = 20
    if (introMsg.length < minMsgLength) {
      return msg.reply(
        `Please write introduction at least ${minMsgLength} characters long!`
      )
    }

    return msg.reply(`Yay successful introduction!`)
  }
})
```

One thing to notice is that you will have to obtain the ID of the `introductions` channel and paste it in your code where I put the placeholder above.
You can find out this ID by going to your Discord server in the Discord app, right-clicking on the `introductions` channel, and clicking on `Copy ID`. For this to work, you will first have to enable the "Developer Mode" (under "User Settings" > "Advanced").

### Removing the "Guest" role upon successful introduction

What is missing is removing the `Guest` role upon successful introduction, so let's do that:

```js {4,24-35} title="bot.js"
...

const INTRODUCTIONS_CHANNEL_ID = "<YOU_WILL_HAVE_TO_FIND_THIS_ON_DISCORD_SERVER>"
const GUEST_ROLE_ID = "<YOU_WILL_HAVE_TO_FIND_THIS_ON_DISCORD_SERVER>"

bot.on('message', async msg => {
  if (msg.content.startsWith('!intro ')) {
    if (msg.channel.id.toString() !== INTRODUCTIONS_CHANNEL_ID) {
      const introductionsChannelName =
        msg.guild.channels.resolve(INTRODUCTIONS_CHANNEL_ID).name
      return msg.reply(
      `Please use !intro command in the ${introductionsChannelName} channel!`
      )
    }

    const introMsg = msg.content.substring('!intro '.length).trim()
    const minMsgLength = 20
    if (introMsg.length < minMsgLength) {
      return msg.reply(
        `Please write introduction at least ${minMsgLength} characters long!`
      )
    }

    const member = msg.guild.member(msg.author)
    try {
      if (member.roles.cache.get(GUEST_ROLE_ID)) {
        await member.roles.remove(GUEST_ROLE_ID)
        return msg.reply(
          'Nice getting to know you! You are no longer a guest' +
          ' and have full access, welcome!'
        )
      }
    } catch (error) {
      return msg.reply(`Error: ${error}`)
    }
  }
})
```

Same as with the ID of the `introductions` channel, now you will also need to find out the ID of the `Guest` role (which you should have created at some point).
You can do it by finding it in the server settings, under the list of roles, right-clicking on it, and then "Copy ID".

This is it! You can now run the bot with
```js
DISCORD_BOT=<TOKEN_OF_YOUR_DISCORD_BOT> node bot.js
```
and if you assign yourself a `Guest` role on the Discord server and then type `!intro Hi this is my introduction, I am happy to be here.` in the `introductions` channel, you should see yourself getting full access together with an appropriate message from your bot.

### Deploying the bot

:::note
Heroku used to offer free apps under certain limits. However, as of November 28, 2022, they ended support for their free tier. https://blog.heroku.com/next-chapter

As such, we have updated our Deployment docs with new recommendations: https://wasp-lang.dev/docs/deploying
:::

While there are many ways to deploy the Discord bot, I will shortly describe how we did it via Heroku.

We created a Heroku app `wasp-discord-bot` and set up the "Automatic deploys" feature on Heroku to automatically deploy every push to the `production` branch (our bot is on Github).

On Heroku, we set the environment variable `DISCORD_BOT` to the token of our bot.

Finally, we added `Procfile` to our project:
```yaml title="Procfile"
worker: node bot.js
```

That is it! On every push to the `production` branch, our bot gets deployed.
