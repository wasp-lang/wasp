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
[ App  ]   VITE v7.3.1  ready in 536 ms
[ App  ]
[ App  ]   ->  Local:   http://localhost:3000/
[ App  ]   ->  Network: http://192.168.1.39:3000/
[ App  ]   ->  Network: http://198.19.249.3:3000/
[ App  ]   ->  Network: http://192.168.215.0:3000/
[ App  ]   ->  press h + enter to show help
```

If you have multiple network interfaces, you'll see multiple Network URLs. Note one of these IPs (you may need to try a few to find the one that works).

## Step 3: Configure Environment Variables

The app won't be fully functional until you configure the environment variables. Edit your `.env.server` file:

```bash title=".env.server"
WASP_WEB_CLIENT_URL=http://192.168.1.39.nip.io:3000
WASP_SERVER_URL=http://192.168.1.39.nip.io:3000
```

Replace `192.168.1.39` with your actual IP address from Step 2.

:::note Why these variables?

Your app serves its pages and its API on the same origin, so both variables hold the same URL. They tell Wasp the public URL your app is reachable at, which it uses for things like OAuth redirects.
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

After saving the environment file, restart your app:

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

1. Make sure the app is accessible on port 3000
2. Verify the hostname you're using is listed in `allowedHosts` in `vite.config.ts`
3. Check the browser console for errors

### OAuth not working

1. Update redirect URIs in your OAuth provider's settings
2. Make sure `WASP_SERVER_URL` uses the nip.io hostname
3. Restart the app after changing environment variables
