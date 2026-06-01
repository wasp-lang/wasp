---
title: Local Network Testing
last_checked_with_versions:
  Wasp: "0.24"
comments: true
---

This guide shows you how to test your Wasp application on other devices (phones, tablets) connected to the same local network during development.

## Prerequisites

- A Wasp project running locally with `wasp start`
- Other devices connected to the same network as your development machine

## Step 1: Start Your App

Run your application normally:

```bash
wasp start
```

`wasp start` auto-detects the LAN address of your machine and configures the dev server so other devices on the same network can connect to it. You'll see a message like:

```
Local network access enabled. Open this URL on another device on the same network:
  http://192.168.1.39.nip.io:3000
Use `wasp start --no-lan` to disable, or `wasp start --host <hostname>` to override the auto-detected hostname.
```

## Step 2: Open the URL on Your Device

On your phone or tablet, open the URL printed by `wasp start`:

```
http://192.168.1.39.nip.io:3000
```

That's it. Both the client and the API call work over the network without any manual configuration.

## Overriding the Hostname

If `wasp start` picks the wrong network interface (this can happen on machines with multiple network adapters, VPNs, or Docker bridges), pass `--host` to specify which hostname or IP to use:

```bash
# Force a specific LAN IP
wasp start --host 192.168.1.50

# Or use a custom hostname (e.g. a mDNS name)
wasp start --host my-laptop.local
```

A bare IP gets a `.nip.io` suffix appended automatically, so OAuth-style integrations keep working. Custom hostnames are used as-is.

## Disabling Local Network Access

If you don't want your dev server exposed on the network, pass `--no-lan`:

```bash
wasp start --no-lan
```

The app will only be reachable at `http://localhost:3000`, just like in earlier versions of Wasp.

## Why Use nip.io?

[nip.io](https://nip.io) is a free DNS service that maps any IP address to a hostname. For example, `192.168.1.39.nip.io` resolves to `192.168.1.39`.

This matters because some Wasp features (like Google OAuth) don't allow plain IP addresses as redirect URLs. Using nip.io gives you a real hostname for your LAN IP without any DNS setup on your side.

## Respecting Your `.env` Files

If you've already set `WASP_WEB_CLIENT_URL`, `WASP_SERVER_URL`, or `REACT_APP_API_URL` in your `.env.server` / `.env.client` files, Wasp won't override them and will print a warning telling you which variable it skipped. If that's not what you want, remove the value from the `.env` file and let `wasp start` populate it.

## OAuth Configuration

If you're using OAuth providers (Google, GitHub, etc.), add your local network URL to the allowed redirect URIs in each provider's configuration:

```
http://192.168.1.39.nip.io:3001/auth/google/callback
```

## Troubleshooting

### Can't access from other devices

1. Make sure both devices are on the same network
2. Check if your firewall is blocking incoming connections on ports 3000 and 3001
3. If you have multiple network interfaces, try overriding with `wasp start --host <ip>`

### API calls failing

1. Confirm the URL in your browser uses the `.nip.io` hostname Wasp printed, not `localhost`
2. Make sure the server is reachable on port 3001 from the same device
3. Check the browser console for CORS errors

### OAuth not working

1. Update redirect URIs in your OAuth provider's settings to match the `.nip.io` URL
2. Make sure you haven't overridden `WASP_SERVER_URL` in `.env.server` with a `localhost` value
