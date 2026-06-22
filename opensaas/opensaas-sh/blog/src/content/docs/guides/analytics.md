---
title: Analytics
banner:
  content: |
    Have an Open SaaS app in production? <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We'll send you some swag! 👕</a>
---
This guide will show you how to integrate analytics for your app. You can choose between [Google Analytics](#google-analytics) and [Plausible](#plausible).

Google Analytics is free, but uses cookies, so you'll probably want/need to implement the [Cookie Consent Modal](/guides/cookie-consent/) when using it.

Plausible is an open-source, privacy-friendly alternative to Google Analytics. **You DO NOT have to use the cookie consent modal** with Plausible, as it does not use cookies. It's also easier to use than Google if you use their hosted service, but be aware it is a paid feature. It is completely free if you want to self-host it, although this comes with some additional setup steps.

If you're looking to add analytics to your blog, you can follow the [Adding Analytics to your Blog](#adding-analytics-to-your-blog) section at the end of this guide.

## Plausible

### Hosted Plausible
Sign up for a hosted Plausible account [here](https://plausible.io/).

Once you've signed up, you'll be taken to your dashboard. Create your site by adding your domain. Your domain is also your `PLAUSIBLE_SITE_ID` in your `.env.server` file. Make sure to add it.

```sh
PLAUSIBLE_SITE_ID=<your domain without www>
```

After adding your domain, you'll be taken to a page with your Plausible script tag. Copy and paste this script tag into the `main.wasp` file's head section. 

```js {7}
app OpenSaaS {
  wasp: {
    version: "^0.13.0"
  },
  title: "My SaaS App",
  head: [
        "<your plausible script tag here>",
  ],
  //...
```

Go back to your Plausible dashboard, click on your username in the top right, and click on the `Settings` tab. Scroll down, find your API key and paste it into your `.env.server` file under the `PLAUSIBLE_API_KEY` variable.

:::note[No Cookies]
Plausible does not use cookies, so you don't need to add it to your [Cookie Consent Modal](/guides/cookie-consent/), hence the script can be added directly to `app.head` in your `main.wasp` file.
:::

### Self-hosted Plausible

Plausible, being an open-source project, allows you to self-host your analytics. This is a great option if you want to keep your data private and not pay for the hosted service.

*coming soon...*
*until then, check out the [official documentation](https://plausible.io/docs)*

:::tip[Contribute!] 
If you'd like to help us write this guide, click the "Edit page" button at the bottom of this page 

As a completely free, open-source project, we appreciate any help 🙏
:::

## Google Analytics

First off, head over to `src/analytics/stats.ts` and switch out the Plausible Provider for Google Analytics so that your [background (cron) job](https://wasp.sh/docs/advanced/jobs) fetches the data from Google Analytics for your [Admin Dashboard](/general/admin-dashboard/):

```ts ins={3} del={2} title="stats.ts"
//...
import { getDailyPageViews, getSources } from './providers/plausibleAnalyticsUtils';
import { getDailyPageViews, getSources } from './providers/googleAnalyticsUtils';

export const calculateDailyStats: DailyStatsJob<never, void> = async (_args, context) => { 
  //...
}
```

Next, make sure you sign up for [Google analytics](https://analytics.google.com/), then go to your `Admin` panel in the bottom of the left sidebar and then create a "Property" for your app.

Once you've created a new Property, some Installation Instructions will pop up. Select `install manually` where you should see a string that looks like this:

```sh title="<your-google-analytics-id>"
 https://www.googletagmanager.com/gtag/js?id=<your-google-analytics-id>
```
and copy and paste the Google Analytics ID into your `.env.client` file to get it working with the [Cookie Consent Modal](/guides/cookie-consent/) provided with this template:

```sh title=".env.client"
REACT_APP_GOOGLE_ANALYTICS_ID=<your-google-analytics-id> # e.g. G-1234567890
```

:::tip[noscript]
In the Installation Instructions, Google Tag Manager might also instruct you to paste the `noscript` code snippet immediately after the opening `<body>` tag.
You should skip this step because this snippet is activated only if users try to browse your app without JavaScript enabled, which is very rare and Wasp needs JS anyway.
:::

Then, set up the Google Analytics API access by following these steps:

1. **Set up a Google Cloud project:** If you haven't already, start by setting up a project in the [Google Cloud Console](https://console.cloud.google.com/).

2. **Enable the Google Analytics API for your project:** Navigate to the "Library" in the Google Cloud Console and search for the "Google Analytics Data API" (for Google Analytics 4 properties) and enable it.

3. **Create credentials:** Now go to the "Credentials" tab within your Google Cloud project, click on `+ credentials`, and create a new service account key. First, give it a name. Then, under "Grant this service account access to project", choose `viewer`.

4. **Create Credentials:** When you go back to `Credentials` page, you should see a new service account listed under "Service Accounts". It will be a long email address to ends with `@your-project-id.iam.gserviceaccount.com`. Click on the service account name to go to the service account details page. 

    - Under "Keys" in the service account details page, click "Add Key" and choose `Create new key`.
  
    - Select "JSON", then click "Create" to download your new service account's JSON key file. Keep this file secure and don't add it to your git repo as it grants access to your Google Analytics data.  
5. **Update your Google Anayltics Settings:** Go back to your Google Analytics dashboard, and click on the `Admin` section in the left sidebar. Under `Property Settings > Property > Property Access Management` Add the service account email address (the one that ends with `@your-project-id.iam.gserviceaccount.com`) and give it `Viewer` permissions.

6. **Encode and add the Credentials:** Add the `client_email` and the `private_key` from your JSON Key file into your `.env.server` file. But be careful! Because Google uses a special PEM private key, you need to first convert the key to base64, otherwise you will run into errors parsing the key. To do this, in a terminal window, run the command below and paste the output into your `.env.server` file under the `GOOGLE_ANALYTICS_PRIVATE_KEY` variable:
    ```sh 
    echo -n "-----BEGIN PRIVATE KEY-----\nMI...A++eK\n-----END PRIVATE KEY-----\n" | base64
    ```
    
7. **Add your Google Analytics Property ID:** You will find the Property ID in your Google Analytics dashboard in the `Admin > Property > Property Settings > Property Details` section of your Google Analytics property (**not** your Google Cloud console). Add this 9-digit number to your `.env.server` file under the `GOOGLE_ANALYTICS_PROPERTY_ID` variable.

## Adding Analytics to your Blog

To add your analytics script to your Astro Starlight blog, all you need to do is modify the `head` property in your `blog/astro.config.mjs` file. 

Below is an example of how to add Google Analytics to your blog:

```js
export default defineConfig({
  site: 'https://opensaas.sh',
  integrations: [
    starlightBlog({ 
      // ...
    }),
    starlight({
      //...
       head: [
        {
          tag: 'script',
          attrs: {
            src: 'https://www.googletagmanager.com/gtag/js?id=<YOUR-GOOGLE-ANALYTICS-ID>',
          },
        },
        {
          tag: 'script',
          content: `
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
        
          gtag('config', '<YOUR-GOOGLE-ANALYTICS-ID>');
          `,
        },
      ],
```
