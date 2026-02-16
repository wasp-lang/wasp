---
comments: true
last_checked_with_versions:
  Wasp: 0.21.0
  Caddy: 2026-01-30
  Ubuntu: 2026-01-30
---

import { SecretGeneratorBlock } from "../../project/SecretGeneratorBlock";

# Simple VPS

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
- **Docker** for running the server and database
- Serving the client with a static file server

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

In your project directory:

```bash
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
  -v postgres_data:/var/lib/postgresql \
  --network myapp-network \
  postgres:18
```

Connect to the database using `psql` to verify it's running:

```bash
docker exec -it myapp-db psql -U postgres
```

List all the tables by typing `\t`. You can exit `psql` by typing in `\q`.

### Step 11: Configure Your Domain

Set up DNS A records pointing to your server IP:

- `@` (root) → your server IP (for `myapp.com`)
- `api` → your server IP (for `api.myapp.com`)

### Step 12: Start the Server

After you built the app with `wasp build`, build the server app Docker image:

```bash
# Navigate to the out directory
cd .wasp/out

# Build the server Docker image
docker build . -t myapp-server
```

Create an `.env.production` environment file in your project directory and add:

| Variable              | Value                                                                  |
| --------------------- | ---------------------------------------------------------------------- |
| `DATABASE_URL`        | `postgresql://postgres:mysecretpassword@myapp-db:5432/myapp`       |
| `JWT_SECRET`          | Random string at least 32 characters long: <SecretGeneratorBlock />    |
| `PORT`                | `3001`                                                                 |
| `WASP_WEB_CLIENT_URL` | `https://<your-domain>`                                                |
| `WASP_SERVER_URL`     | `https://api.<your-domain>`                                            |


Add any other environment variables your app needs (from `.env.server`).

Start the server container:

```bash
docker run -d \
  --name myapp-server \
  --env-file .env.production \
  -p 127.0.0.1:3001:3001 \
  --network myapp-network \
  myapp-server
```

:::note
We bind to `127.0.0.1:5432` to ensure the server is only accessible from the server itself, not from the internet.
:::

Verify it's running:

```bash
curl -I http://localhost:3001
```

You should see a `200 OK` HTTP status code.

### Step 13: Build the Client

In the project directory run:

```bash
REACT_APP_API_URL=https://api.myapp.com npx vite build
```

Copy the built files to a serving directory:
```bash
sudo mkdir -p /var/www
sudo cp -R .wasp/out/web-app/build/* /var/www/
sudo chown -R caddy:caddy /var/www
```

### Step 14: Configure Caddy

Edit the Caddyfile at `/etc/caddy/Caddyfile`:
```caddyfile
myapp.com {
    root * /var/www
    encode gzip
    try_files {path} /index.html
    file_server
}

api.myapp.com {
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
SERVER_APP_NAME="myapp-server"
SERVER_APP_URL=https://api.myapp.com

echo "Pulling latest changes..."
cd ~/"$APP_DIR"
git pull

echo "Building Wasp project..."
wasp build

echo "Stopping existing server..."
docker container stop $SERVER_APP_NAME && docker container rm $SERVER_APP_NAME || true

echo "Building Docker image..."
cd .wasp/out/
docker build . -t $SERVER_APP_NAME

echo "Starting new server..."
cd ~/"$APP_DIR"
docker run -d --name $SERVER_APP_NAME --env-file .env.production -p 127.0.0.1:3001:3001 --network myapp-network $SERVER_APP_NAME

echo "Building client..."
REACT_APP_API_URL=$SERVER_APP_URL npx vite build

echo "Copying new client files..."
rm -r /var/www/*
cp -R .wasp/out/web-app/build/* /var/www
```

Make it executable and run:

```bash
chmod +x redeploy.sh
./redeploy.sh
```

### Minimizing Downtime

Configure Caddy to retry connections during restarts:

```caddyfile
api.myapp.com {
    reverse_proxy localhost:3001 {
        health_uri /
        lb_try_duration 15s
    }
}
```

This makes Caddy wait up to 15 seconds for the server to become available again.
