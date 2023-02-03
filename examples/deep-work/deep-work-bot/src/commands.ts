import { Message, MessageEmbed, MessageCollector } from 'discord.js';
import config from './config';
import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

const { prefix } = config;

type Command = {
  description: string;
  format: string;
  aliases?: string[];
};

const commands: { [name: string]: Command } = {
  deepworkhelp: {
    description: 'Shows the list of commands and their details.',
    format: 'deepworkhelp',
  },
  deepwork: {
    description: 'Log a Deep Work session',
    format: 'deepwork | deepwork <minutes>',
  },
  whois: {
    description: 'Who is in Deep Work mode',
    format: 'whois',
  },
  timeleft: {
    description: 'Time Left in your session',
    format: 'timeleft',
  },
  url: {
    description: 'The Deep Work Dashboard',
    format: 'url',
  },
};

interface session {
  username: string;
  timeStarted: number;
  minutes: string;
}

const sessionCache: Map<string, string> = new Map();

async function saveDeepWorkSession(message: Message, time: number) {
  const currentSession = sessionCache.get(message.author.id);
  if (currentSession) sessionCache.delete(message.author.id);

  const minutesInMilliseconds = time * 60 * 1000;
  const sessionObj: session = {
    username: message.author.username,
    timeStarted: Date.now(),
    minutes: time.toString(),
  };

  const session = sessionCache.set(message.author.id, JSON.stringify(sessionObj));

  console.log('session set', session);

  setTimeout(async () => {
    // Send a message to the user to let them know the session is over
    await message.channel.send(
      `I'm proud of you ${message.author.username}! 🧙‍♂️ You DeepWorked for ${time} minutes.`
    );
    await prisma.work.create({
      data: {
        username: message.author.username,
        minutes: time.toString(),
        timeStarted: new Date().toISOString(),
        user: { connect: { userId: message.author.id } },
      },
    });
    sessionCache.delete(message.author.id);
  }, minutesInMilliseconds);

}

export async function deepWorkCommand(message: Message) {
  const isUserValid: boolean = await checkValidUser(message);
  if (!isUserValid) {
    await message.channel.send(
      `🪧 Please register with the tracking app first before trying to log hours -- ${process.env.APP_URL} `
    );
    return;
  }
  const usersWorkTime = message.content.split(' ').length > 1 ? parseInt(message.content.split(' ')[1]) : 0 ;

  console.log('usersWorkTime: ', usersWorkTime)

  if (!usersWorkTime) {
    // Send a message asking the user for input
    await message.channel.send('⏳ How Long would you like your session to be (in minutes)?:');

    // Create a message collector to listen for the user's response
    const collector = new MessageCollector(message.channel, {
      filter: (m) => m.author.id === message.author.id,
      time: 20000,
    });

    // Wait for the user's response
    collector.on('collect', async (userMessage: Message) => {
      if (typeof parseInt(userMessage.content) !== 'number') {
        await message.channel.send('Please enter a number');
      } else {
        await message.channel.send(
          `Nice 🧙‍♂️! You want to work for: "${userMessage.content} minutes" \n\n Consider turning off other app notifications and distractions`
        );
        await saveDeepWorkSession(message, parseInt(userMessage.content));
        collector.stop();
      }
    });

    // If the user doesn't respond within 20 seconds, send a message and stop the collector
    collector.on('end', async (_, reason: string) => {
      if (reason === 'time') {
        await message.channel.send('You took too long to respond.');
      }
    });
  } else {
    await message.channel.send(
      `Nice 🧙‍♂️! You want to work for: "${usersWorkTime} minutes" \n\n Consider turning off other app notifications and distractions`
    );
    await saveDeepWorkSession(message, usersWorkTime);
  } 
}

export async function deepWorkTimeLeft(message: Message) {
  const sessions = sessionCache.entries();
  console.log('current sessions: ', sessions);

  let timeLeft = 'no session';
  if (sessions) {
    sessionCache.forEach((sess, key) => {
      const session = JSON.parse(sess);
      if (message.author.id === key) {
        timeLeft = (Number(session.minutes) - (Date.now() - session.timeStarted) / 1000 / 60).toString();
      }
    });
  }
  await message.channel.send(`${timeLeft} minutes left to DeepWork.`);
}

async function checkValidUser(message: Message) {
  const user = await prisma.user.findUnique({
    where: {
      username: message.author.username,
    },
  });

  if (!user) return false;

  if (!user.userId) {
    await prisma.user.update({
      where: {
        username: message.author.username,
      },
      data: {
        userId: message.author.id,
      },
    });
  }
  return true;
}

export async function deepWorkWorkingNow(message: Message) {
  let sessionsArr = Array<string>();
  sessionCache.forEach((sess) => {
    console.log('sess: ', sess);
    const session = JSON.parse(sess);
    sessionsArr.push(session.username);
  });
  await message.channel.send(`Current Deep Workers: ${sessionsArr ? `💭 ${sessionsArr.join(',')}` : '🧹 nobody'}`);
}

export async function deepWorkURL(message: Message) {
  await message.channel.send(`${process.env.APP_URL}`);
}

export function helpCommand(message: Message) {
  const footerText = message.author.tag;
  const footerIcon = message.author.displayAvatarURL();
  const embed = new MessageEmbed()
    .setTitle('HELP MENU')
    .setColor('GREEN')
    .setFooter({ text: footerText, iconURL: footerIcon });

  for (const commandName of Object.keys(commands)) {
    const command = commands[commandName];
    let desc = command.description + '\n\n';
    if (command.aliases) desc += `**Aliases :** ${command.aliases.join(', ')}\n\n`;
    desc += '**Format**\n```\n' + prefix + command.format + '```';

    embed.addField(commandName, desc, false);
  }

  return embed;
}
