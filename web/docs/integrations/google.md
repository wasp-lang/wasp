---
title: Google Integrations
---

import useBaseUrl from '@docusaurus/useBaseUrl';

# Google Integrations

## Google Auth

In order to use Google as an authentication method (covered [here](/docs/language/features#google)), you'll first need to create a Google project and provide Wasp with your client key and secret. Here is how to do so:

- Create a Google Cloud Platform account if you do not already have one: https://cloud.google.com/
- Create and configure a new Google project here: https://console.cloud.google.com/home/dashboard
  - ![Google Console Screenshot 1](../../static/img/integrations-google-1.jpg)
  - ![Google Console Screenshot 2](../../static/img/integrations-google-2.jpg)
- Search for `OAuth` in the top bar, click on `OAuth consent screen`
  - ![Google Console Screenshot 3](../../static/img/integrations-google-3.jpg)
  - Select what type of app you want, we will go External
    - ![Google Console Screenshot 4](../../static/img/integrations-google-4.jpg)
  - Fill out applicable information on Page 1
    - ![Google Console Screenshot 5](../../static/img/integrations-google-5.jpg)
  - On Page 2, Scopes, search for `email`
    - Select the `userinfo.email` row
      - ![Google Console Screenshot 6](../../static/img/integrations-google-6.jpg)
      - ![Google Console Screenshot 7](../../static/img/integrations-google-7.jpg)
      - ![Google Console Screenshot 8](../../static/img/integrations-google-8.jpg)
  - Add any test users you want on Page 3
    - ![Google Console Screenshot 9](../../static/img/integrations-google-9.jpg)
- Next, click `Credentials`
  - ![Google Console Screenshot 10](../../static/img/integrations-google-10.jpg)
  - Select `+ Create Credentials`
  - Select `OAuth client ID`
    - ![Google Console Screenshot 11](../../static/img/integrations-google-11.jpg)
  - Complete the form
    - ![Google Console Screenshot 12](../../static/img/integrations-google-12.jpg)
  - Under Authorized URIs, put in: `http://localhost:3000/auth/redirect/google`
    - ![Google Console Screenshot 13](../../static/img/integrations-google-13.jpg)
    - You can optionally put in any Heroku or other PaaS URL where your API server will be deployed
      - For example: `https://someotherhost.com/auth/redirect/google`
  - When you save, you can click the Edit icon and your credentials will be shown
    - ![Google Console Screenshot 14](../../static/img/integrations-google-14.jpg)
- Copy your Client ID and Client secret, and expose them as environment variables named `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET` wherever your app is running
