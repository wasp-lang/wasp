
# Deep Work Dashboard

<img src="https://i.imgur.com/or7nCku.png" width="500px">

This is a sweet time-tracking dashboard app built with [Wasp](https://wasp-lang.dev) (a fullstack React and NodeJS framework) and recharts npm package. It displays the amount of "Deep Work" time logged by a user via a Discord Bot.

<img src="https://i.imgur.com/oB8B3T8.png">
<img src="https://i.imgur.com/qCx4CeB.png">

## Why a Deep Work Bot & Dashboard?

As a developer in a remote team, Discord is fundamental to internal and community communication. But it can also be very distracting at times. That's why I was looking for a way to help me use a tool that often distracts me in a more deliberate and conscious way.

## How it works 

The idea is that you deliberately set your Discord status to "Do Not Disturb" when you want to focus on a task. The bot will then log the time to the DB, and the dashboard app will parse that data into some nice charts.

Charts were made using the [recharts](https://recharts.org/en-US/) npm package. Charts can be viewed by day, week, month, or year. 

## Setup

First of all, set up the bot. The bot code and instructions can be found in the parent directory.

Next, make sure you [install Wasp](https://wasp-lang.dev/docs#2-installation) by running `curl -sSL https://get.wasp-lang.dev/installer.sh | sh` in your terminal.

Then, run `wasp start` in the root directory to start the app. The Frontend will be running on `localhost:3000` while the server runs on port `3001`.
It may be necessary to kill the app and then run `wasp db migrate-dev` to migrate the database, before starting it again.

Wasp takes care of building your nodejs server and react app, as well as installing all the dependencies.
The configuration of the Wasp app can be found in the `main.wasp` file. Check out the [Wasp docs](https://wasp-lang.dev/docs) for more info.

## Registering and loggin in Users

Wasp has built in Auth support so username and password auth is already taken care of :)

IMPORTANT! make sure that when registering a user, you **use the same username as the one for your Discord user (case sensitive)**. This is because the dashboard app uses the Discord username to query the DB for the user's data, which was.

