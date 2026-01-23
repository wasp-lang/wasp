---
comments: true
---

# VPS / Self-hosted

## Deploy Wasp to a VPS

This guide shows you how to deploy a Wasp application directly to a VPS (Virtual Private Server) using Docker and a reverse proxy.

### Prerequisites

- A VPS (e.g., from [Hetzner](https://hetzner.cloud/), DigitalOcean, Linode, etc.)
- A domain name
- Basic familiarity with SSH and Linux commands

### Architecture Overview

Our deployment setup includes:

- **Ubuntu LTS** as the operating system
- **Caddy** as a reverse proxy for HTTPS and domain handling
- **Docker** for running the server and database
- Static file serving for the client

### Step 1: Connect to Your Server

Connect to your server via SSH:

```bash
ssh <username>@<server-ip>
```

Usually the username is `root` if the provider doesn't specify otherwise.

### Step 2: Install Caddy (Reverse Proxy)

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

## Allow SSH connections (do this BEFORE enabling UFW!)
ufw allow ssh
ufw show added

## Enable the firewall
ufw enable

## Allow HTTP and HTTPS
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

Get the public key:

```bash
cat ~/.ssh/id_rsa.pub
```

Add this key as a deploy key at `https://github.com/<username>/<repo-name>/settings/keys/new`.

### Step 6: Clone Your Repository

```bash
git clone git@github.com: < username > / < repo-name > .git
```

### Step 7: Install Wasp CLI

Install the Wasp CLI:

```bash
curl -sSL https://get.wasp.sh/installer.sh | sh
```

Add Wasp to your PATH by adding this line to `~/.bashrc`:

```bash
export PATH=$PATH:~/.local/bin
```

Reload your shell:

```bash
source ~/.bashrc
```

### Step 8: Install Node.js

Install Node.js using nvm:

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
source ~/.bashrc
nvm install 20
```

### Step 9: Build the Application

In your project directory:

```bash
## Build the Wasp app
wasp build

## Navigate to the build directory
cd .wasp/out

## Build the Docker image
docker build . -t wasp-app
```

Replace `wasp-app` with your preferred image name.

### Step 10: Start the Database

Create a Docker network:

```bash
docker network create wasp-network
```

Start PostgreSQL:

```bash
docker run -d \
  --name db \
  -e POSTGRES_PASSWORD=mysecretpassword \
  -v postgres_data:/var/lib/postgresql/data \
  -p 127.0.0.1:5432:5432 \
  --network wasp-network \
  postgres:16
```

:::note
We bind to `127.0.0.1:5432` to ensure the database is only accessible from the server itself, not from the internet.
:::

To connect to the database:

```bash
docker exec -it db psql -U postgres
```

### Step 11: Configure Your Domain

Set up DNS A records pointing to your server IP:

- `@` (root) → your server IP (for `myapp.com`)
- `api` → your server IP (for `api.myapp.com`)

### Step 12: Start the Server

Create an environment file in your project directory:

```bash title=".env.production"
PORT=3001
DATABASE_URL=postgresql://postgres:mysecretpassword@db:5432/postgres
WASP_WEB_CLIENT_URL=https://myapp.com
WASP_SERVER_URL=https://api.myapp.com
JWT_SECRET=<generate-with-https://djecrety.ir/>

## Add any other env vars your app needs
```

Start the server container:

```bash
docker run -d \
  --name wasp-app \
  --env-file .env.production \
  -p 127.0.0.1:3001:3001 \
  --network wasp-network \
  wasp-app
```

Verify it's running:

```bash
curl localhost:3001
```

### Step 13: Build the Client

```bash
cd .wasp/out/web-app
npm install
REACT_APP_API_URL=https://api.myapp.com npm run build
```

Copy the built files to a serving directory:

```bash
mkdir -p ~/client
cp -R .wasp/out/web-app/build/* ~/client/
```

### Step 14: Configure Caddy

Edit the Caddyfile at `/etc/caddy/Caddyfile`:

```caddyfile
myapp.com {
    root * /root/client
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
caddy reload --config /etc/caddy/Caddyfile
```

Your app should now be accessible at `https://myapp.com`!

### Redeploying Updates

Create a deployment script:

```bash title="redeploy.sh"
##!/bin/bash

set -e

APP_NAME="your-app-name"

echo "Pulling latest changes..."
cd ~/"$APP_NAME"
git pull

echo "Building Wasp project..."
wasp build

echo "Stopping existing container..."
docker container stop wasp-app && docker container rm wasp-app || true

echo "Building Docker image..."
cd .wasp/out/
docker build . -t wasp-app

echo "Starting new container..."
cd ~/\"$APP_NAME\"
docker run -d --name wasp-app --env-file .env.production -p 127.0.0.1:3001:3001 --network wasp-network wasp-app

echo "Building client..."
cd .wasp/out/web-app/
npm install
REACT_APP_API_URL=https://api.myapp.com npm run build

echo "Copying client files..."
cp -R build/* ~/client/

echo "Deployment complete!"
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

### Using Cloudflare CDN (Optional)

To use Cloudflare:

1. Move your domain's nameservers to Cloudflare
2. Add `A` records in `DNS Only` mode first (so Caddy can obtain SSL certificates)
3. Once everything works, switch to `Proxy` mode
4. Use `Full (Strict)` SSL mode in Cloudflare
