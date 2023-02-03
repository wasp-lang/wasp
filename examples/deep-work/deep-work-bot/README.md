# Deep Work Discord Bot

<img src="https://i.imgur.com/oB8B3T8.png">
<img src="https://i.imgur.com/qCx4CeB.png">

# Why a Deep Work Bot?

As a developer in a remote team, Discord is fundamental to internal and community communication. But it can also be very distracting at times. That's why I was looking for a way to help me use a tool that often distracts me in a more deliberate and conscious way.

# How does it work?

The idea is that you deliberately set your Discord status to "Do Not Disturb" when you want to focus on a task. The bot will then log the time to the DB, and the dashboard app will parse that data into some nice charts. The dashboard app code can be found in the parent directory.

The bot specifically listens for a certain Discord ROLE_ID to change status to "Do Not Disturb", or for such a user to send the command `!deepwork` to the bot.

<img src="https://i.imgur.com/or7nCku.png" width="500px">

## Setup

First of all, go to [discord developer dashboard](https://discord.com/developers/applications/) and choose the bot you want to host, go to the bot tab, and click on `Regenerate token`, and then copy the token.

After this, go to the "Lock"-like tab on replit and here, in the `key` enter `DISCORD_TOKEN` and in the `value` field, enter the token you copied from discord developer dashboard.
![image](https://i.postimg.cc/k5tMPRpk/image.png)

---

## Configuration

You can modify any kind of configuration in `src/config.ts`. DO NOT change any files in the `dist` directory as they are all generated files and should not be manually edited.

---

## Commands

You can edit these commands and/or add more in `src/index.ts`. After creating the command in `src/index.ts`, go to `src/commands.ts` and to the json, add a new entry with the format
```ts
{
  ...,
  'command-name': {
    aliases: ['these', 'are', 'optional'],
    description: 'This command does xyz...',
    format: 'command-name <my-args>'
  }
}
```

Here, `command-name` is the name of your new command, `aliases` is an array of the aliases (other ways to invoke this command). The `description` and `format` are required fields to display the command properly in the help embed.

> **NOTE :** This is solely to add the command to the help embed, this does not affect the working of commands whatsoever, this only adds the command to help embeds.

---

## Testing 

If you want to test your bot, add ROLE_ID and CHANNEL_ID from a test server, rather then the one you want to use in production, in your `src/config.ts` file. 

## Hosting

Finally, once you are done writing your bot, run `npm run build`. The build directory can be hosted on a number of platforms.

