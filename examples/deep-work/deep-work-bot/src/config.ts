import { Intents } from 'discord.js';

export default {
  prefix: '!',
  token: process.env.DISCORD_TOKEN,
  intents: [
    Intents.FLAGS.GUILDS,
    Intents.FLAGS.GUILD_MESSAGES,
    Intents.FLAGS.GUILD_PRESENCES
  ],
  testRoleId: '',
  testChannelId: '',
}
