---
title: GitHub Integrations
---

import useBaseUrl from '@docusaurus/useBaseUrl';

# GitHub Integrations

## GitHub Auth

To use GitHub as an authentication method (covered [here](/docs/language/features#github)), you'll first need to create a GitHub OAuth App and provide Wasp with your client key and secret. Here is how to do so:

1. Log into your GitHub account and navigate to: https://github.com/settings/developers
2. Select "New OAuth App"
3. Supply required information

  <img alt="GitHub Applications Screenshot"
      src={useBaseUrl('img/integrations-github-1.png')}
      width="400px"
  />

  - For "Authorization callback URL", if you just want to test your local app, put in: `http://localhost:3000/auth/login/github`
  - Once you know on which URL your API server will be deployed, you can create a new app with that URL instead.
      - For example: `https://someotherhost.com/auth/login/github`
4. Hit "Register application"
5. Copy your Client ID and Client secret, and expose them as environment variables named `GITHUB_CLIENT_ID` and `GITHUB_CLIENT_SECRET` wherever your app is running
