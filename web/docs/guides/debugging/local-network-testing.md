---
title: Local Network Testing
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.24" }} />

This guide shows you how to test your Wasp application on other devices (phones, tablets) connected to the same local network during development.

## Prerequisites

- A Wasp project running locally with `wasp start`
- Other devices connected to the same network as your development machine

## Step 1: Start Your App

Run your application normally:

```bash
wasp start
```

## Step 2: Find Your Network URL

Look for the network URLs in Wasp's terminal output:

```
[ Client ]   VITE v7.3.1  ready in 536 ms
[ Client ]
[ Client ]   ->  Local:   http://localhost:3000/
[ Client ]   ->  Network: http://192.168.1.39:3000/
[ Client ]   ->  Network: http://198.19.249.3:3000/
[ Client ]   ->  Network: http://192.168.215.0:3000/
[ Client ]   ->  press h + enter to show help
```

If you have multiple network interfaces, you'll see multiple Network URLs. Note one of these IPs (you may need to try a few to find the one that works).

## Step 3: Configure Environment Variables

The app won't be fully functional until you configure the environment variables. Edit your environment files:

### .env.server

```bash title=".env.server"
WASP_SERVER_URL=http://192.168.1.39.nip.io:3000
```

Replace `192.168.1.39` with your actual IP address from Step 2.

:::note Why this variable?

The client dev server on port `3000` proxies Wasp's routes to the server, so other devices only need to reach port `3000` and the client finds the server on its own origin. `WASP_SERVER_URL` is the one thing Wasp can't guess: it's the public origin it puts into OAuth redirect URIs, and it must match what the browser sees. `WASP_WEB_CLIENT_URL` defaults to it.
:::

:::note Split mode
In [split mode](../../deployment/intro.md#deployment-modes) there is no proxy: other devices have to reach both ports, and you set `WASP_SERVER_URL=http://192.168.1.39.nip.io:3001`, `WASP_WEB_CLIENT_URL=http://192.168.1.39.nip.io:3000` in `.env.server` and `REACT_APP_API_URL=http://192.168.1.39.nip.io:3001` in `.env.client`.
:::

## Step 4: Allow the Host in Vite Config

By default, Vite blocks requests from hostnames other than `localhost`. Since we're using a `.nip.io` hostname, you need to explicitly allow it.

Add `allowedHosts` to the `server` section in your `vite.config.ts`:

```ts title="vite.config.ts"
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  server: {
    // highlight-next-line
    allowedHosts: ["192.168.1.39.nip.io"],
  },
  plugins: [wasp()],
});
```

Replace `192.168.1.39.nip.io` with the hostname matching your IP from Step 2.

## Step 5: Restart and Test

After saving the environment files, restart your app:

```bash
wasp start
```

On your phone or tablet, open the URL with the `.nip.io` suffix:

```
http://192.168.1.39.nip.io:3000
```

## Why Use nip.io?

[nip.io](https://nip.io) is a free DNS service that maps any IP address to a hostname. For example, `192.168.1.39.nip.io` resolves to `192.168.1.39`.

This is necessary because some Wasp features (like Google OAuth) don't allow plain IP addresses. Using nip.io provides a proper hostname without any configuration.

:::tip
You can skip nip.io if you're not using features that require proper hostnames, but using it has no downside and ensures everything works correctly.
:::

## OAuth Configuration

If you're using OAuth providers (Google, GitHub, etc.), remember to add your local network URLs to the allowed redirect URIs in each provider's configuration:

```
http://192.168.1.39.nip.io:3000/auth/google/callback
```

## Troubleshooting

### Can't access from other devices

1. Make sure both devices are on the same network
2. Check if your firewall is blocking incoming connections on port 3000
3. Try different Network URLs if you have multiple

### API calls failing

1. Make sure you opened the app through the `.nip.io` hostname on port 3000, not the server port
2. Check the terminal for proxy errors from the client dev server
3. In split mode, verify `REACT_APP_API_URL` is set correctly in `.env.client`, that the server is reachable on port 3001, and check the browser console for CORS errors

### OAuth not working

1. Update redirect URIs in your OAuth provider's settings
2. Make sure `WASP_SERVER_URL` uses the nip.io hostname
3. Restart the server after changing environment variables
