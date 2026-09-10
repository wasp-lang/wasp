---
title: Overview
---

import { WaspDeployProvidersGrid } from './WaspDeployProvidersGrid';

Wasp CLI can deploy your full-stack application with a single command.
The command automates the manual deployment process and is the recommended way of deploying Wasp apps.

It looks like this:
```shell
wasp deploy <provider> launch my-wasp-app
```

The `wasp deploy` command sets up all the necessary services on the provider, builds your Wasp app, and deploys it. It creates one app (or service) for your Wasp app, which serves both the server and the client, and one for the database.

### Supported Providers

Wasp Deploy supports automated deployment to the following providers:

<WaspDeployProvidersGrid />