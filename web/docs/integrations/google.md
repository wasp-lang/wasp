---
title: Google Integrations
---

import useBaseUrl from '@docusaurus/useBaseUrl';

# Google Integrations

## Google Auth

To use Google as an authentication method (covered [here](/docs/language/features#google)), you'll first need to create a Google project and provide Wasp with your client key and secret. Here is how to do so:

1. Create a Google Cloud Platform account if you do not already have one: https://cloud.google.com/
2. Create and configure a new Google project here: https://console.cloud.google.com/home/dashboard

  ![Google Console Screenshot 1](../../static/img/integrations-google-1.jpg)

  ![Google Console Screenshot 2](../../static/img/integrations-google-2.jpg)

3. Search for `OAuth` in the top bar, click on `OAuth consent screen`

  ![Google Console Screenshot 3](../../static/img/integrations-google-3.jpg)

  - Select what type of app you want, we will go External

    ![Google Console Screenshot 4](../../static/img/integrations-google-4.jpg)

  - Fill out applicable information on Page 1

    ![Google Console Screenshot 5](../../static/img/integrations-google-5.jpg)

  - On Page 2, Scopes, you should select `userinfo.profile`. You can optionally search for other things, like `email`.

    ![Google Console Screenshot 6](../../static/img/integrations-google-6.jpg)

    ![Google Console Screenshot 7](../../static/img/integrations-google-7.jpg)

    ![Google Console Screenshot 8](../../static/img/integrations-google-8.jpg)

  - Add any test users you want on Page 3

    ![Google Console Screenshot 9](../../static/img/integrations-google-9.jpg)

4. Next, click `Credentials`

  ![Google Console Screenshot 10](../../static/img/integrations-google-10.jpg)

  - Select `+ Create Credentials`
  - Select `OAuth client ID`

    ![Google Console Screenshot 11](../../static/img/integrations-google-11.jpg)

  - Complete the form

    ![Google Console Screenshot 12](../../static/img/integrations-google-12.jpg)

  - Under Authorized redirect URIs, put in: `http://localhost:3000/auth/login/google`

    ![Google Console Screenshot 13](../../static/img/integrations-google-13.jpg)

    - Once you know on which URL(s) your API server will be deployed, also add those URL(s)
      - For example: `https://someotherhost.com/auth/login/google`
  - When you save, you can click the Edit icon and your credentials will be shown

    ![Google Console Screenshot 14](../../static/img/integrations-google-14.jpg)

5. Copy your Client ID and Client secret, and expose them as environment variables named `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET` wherever your app is running
