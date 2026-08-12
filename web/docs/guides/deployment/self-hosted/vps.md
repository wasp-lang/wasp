---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import { SecretGeneratorBlock } from "../../../project/SecretGeneratorBlock";

# Simple VPS

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", Caddy: new Date("2026-01-30"), Ubuntu: new Date("2026-01-30") }} />

## Deploy Wasp to a VPS

This guide shows you how to deploy a Wasp application directly to a VPS (Virtual Private Server) using Docker and a reverse proxy.

### Prerequisites

- A VPS (e.g., from Hetzner, DigitalOcean, Linode, etc.)
- A domain name
- Basic familiarity with SSH and Linux commands

### Architecture Overview

Our deployment setup includes:

- **Ubuntu LTS** as the operating system
- **Caddy** as a reverse proxy for HTTPS and domain handling
- **Docker** for running the app and the database

`wasp build` gives you a single Docker image that contains your whole app: its pages, its static assets, its API and its websockets. It all runs on one port behind one domain, so there is nothing to host separately.

### Step 1: Connect to Your Server

Connect to your server via SSH:

```bash
ssh <username>@<server-ip>
```

Usually the username is `root` if the provider doesn't specify otherwise.

### Step 2: Install Caddy

First, update your package list:

```bash
apt update
```

If Apache is installed, you may need to [uninstall it](https://askubuntu.com/a/387793) first. Check with `which apache2`.

Install Caddy following the [official Ubuntu instructions](https://caddyserver.com/docs/install#debian-ubuntu-raspbian).

After installation, visit your server's IP to see the Caddy welcome message.

### Step 3: Set Up the Firewall

Configure UFW to only allow necessary connections:

```bash
ufw default deny incoming
ufw default allow outgoing

# Allow SSH connections (do this BEFORE enabling UFW!)
ufw allow ssh
ufw show added

# Enable the firewall
ufw enable

# Allow HTTP and HTTPS
ufw allow http
ufw allow https
```

### Step 4: Install Docker

Follow the [official Docker installation guide for Ubuntu](https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository).

Verify the installation:

```bash
docker run hello-world
```

### Step 5: Set Up GitHub Deploy Key

To clone from a private repository, generate an SSH key on your server:

```bash
ssh-keygen
```

Get the public key (the filename might very depending on the key type):

```bash
cat ~/.ssh/id_ed25519.pub
```

Add this key as a deploy key at `https://github.com/<username>/<repo-name>/settings/keys/new`.

### Step 6: Clone Your Repository

```bash
git clone git@github.com:<username>/<repo-name>.git
```

### Step 7: Install Node.js

Install Node.js using nvm:

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
source ~/.bashrc
nvm install {minimumNodeJsVersion}
```

### Step 8: Install Wasp CLI

Install the Wasp CLI:

```bash
npm i -g @wasp.sh/wasp-cli
```

Add Wasp to your PATH by adding this line to `~/.bashrc`:

```bash
export PATH=$PATH:~/.local/bin
```

Reload your shell:

```bash
source ~/.bashrc
```

Confirm the Wasp CLI works by running:

```bash
wasp version
```

### Step 9: Build the Application

In your project directory, install dependencies and build the app:

```bash
wasp install
wasp build
```

### Step 10: Start the Database

Create a Docker network:

```bash
docker network create myapp-network
```

Start PostgreSQL:

```bash
docker run -d \
  --name myapp-db \
  -e POSTGRES_PASSWORD=mysecretpassword \
  -e POSTGRES_DB=myapp \
  -v postgres_data:/var/lib/postgresql \
  --network myapp-network \
  postgres:18
```

Connect to the database using `psql` to verify it's running:

```bash
docker exec -it myapp-db psql -U postgres -d myapp
```

Verify you are connected to the `myapp` database by typing `\conninfo`. You can exit `psql` by typing in `\q`.

### Step 11: Configure Your Domain

Set up a DNS A record pointing to your server IP:

- `@` (root) → your server IP (for `myapp.com`)

### Step 12: Start the App

After you built the app with `wasp build`, build its Docker image:

```bash
docker build .wasp/out -t myapp
```

:::note Client environment variables
Environment variables prefixed with `REACT_APP_` end up inside your app's pages and assets, so they have to be there when the image is built, not when it runs. Pass them with the `WASP_CLIENT_ENV` build argument, as shell assignments, one per line:

```bash
docker build --build-arg WASP_CLIENT_ENV="REACT_APP_EXAMPLE='value'" .wasp/out -t myapp
```
:::

Create an `.env.production` environment file in your project directory and add:

| Variable          | Value                                                               |
| ----------------- | ------------------------------------------------------------------- |
| `DATABASE_URL`    | `postgresql://postgres:mysecretpassword@myapp-db:5432/myapp`        |
| `JWT_SECRET`      | Random string at least 32 characters long: <SecretGeneratorBlock /> |
| `PORT`            | `3001`                                                              |
| `WASP_SERVER_URL` | `https://<your-domain>`                                             |

Add any other environment variables your app needs (from `.env.server`).

:::tip
`WASP_WEB_CLIENT_URL` defaults to `WASP_SERVER_URL`, and your app's pages and its API are on the same origin now, so setting `WASP_SERVER_URL` to your domain is all it takes.
:::

Start the app container:

```bash
docker run -d \
  --name myapp \
  --env-file .env.production \
  -p 127.0.0.1:3001:3001 \
  --network myapp-network \
  myapp
```

:::note
We bind the app to `127.0.0.1:3001` to ensure it is only accessible from the server itself, not directly from the internet. Caddy is what exposes it to the world.
:::

The container applies your database migrations on startup and then starts the app.

Verify it's running:

```bash
curl -I http://localhost:3001/_wasp/health
```

You should see a `200 OK` HTTP status code.

### Step 13: Configure Caddy

Edit the Caddyfile at `/etc/caddy/Caddyfile`:
```caddyfile
myapp.com {
    reverse_proxy localhost:3001
}
```

Reload Caddy:
```bash
sudo systemctl reload caddy
```

Your app should now be accessible at `https://myapp.com`!

### Redeploying Updates

Create a deployment script:

```bash title="redeploy.sh"
#!/bin/bash

set -e

APP_DIR="your-app-name"
APP_NAME="myapp"

echo "Pulling latest changes..."
cd ~/"$APP_DIR"
git pull

echo "Building Wasp project..."
wasp build

echo "Building Docker image..."
docker build .wasp/out -t $APP_NAME

echo "Stopping existing container..."
docker container stop $APP_NAME && docker container rm $APP_NAME || true

echo "Starting new container..."
docker run -d --name $APP_NAME --env-file .env.production -p 127.0.0.1:3001:3001 --network myapp-network $APP_NAME
```

Make it executable and run:

```bash
chmod +x redeploy.sh
./redeploy.sh
```

### Minimizing Downtime

Configure Caddy to retry connections during restarts:

```caddyfile
myapp.com {
    reverse_proxy localhost:3001 {
        health_uri /_wasp/health
        lb_try_duration 15s
    }
}
```

This makes Caddy wait up to 15 seconds for the app to become available again.
