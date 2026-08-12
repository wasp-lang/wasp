---
title: Extras
---

In this section, we will cover some additional topics that are important for deploying Wasp apps in production.

### Custom domain setup

Your app is served from one domain, which is what your users visit from their browsers, and where your app's API and WebSockets live too.

#### How to do it?

It's usually a two-step process:

1. Set up the **DNS records** for the domain.

   This will depend on your hosting provider. You can usually do this by adding an `A` record in your DNS settings that points to the app's IPv4 address. You often set the `AAAA` record for IPv6 address as well. Some hosting providers ask you to set the `CNAME` record instead of the `A` and `AAAA` records.

:::note Using `wasp deploy`?

Check out how to set up custom domains with [Fly.io](./deployment-methods/wasp-deploy/fly.md#custom-domain) or [Railway](./deployment-methods/wasp-deploy/railway.md#custom-domain).

:::

2. Set up the **environment variables** for the app.

   Your app builds links from its own URL (the ones in the emails it sends, and the ones it redirects OAuth logins to), so it has to know what that URL is. [Set it](./env-vars.md#server-env-vars) as `WASP_SERVER_URL`:

   ```bash
   WASP_SERVER_URL=https://myapp.com
   ```

   <small>
     Learn more about server env variables in the [env vars section](../project/env-vars.md#server-general-configuration).
   </small>

### DDoS protection and CDN recommendations

When deploying your Wasp app, you might want to consider using a Content Delivery Network (CDN) and DDoS protection service to improve the performance and security of your app:

1. **Content Delivery Network (CDN)** is a network of servers distributed worldwide that caches static assets like images, CSS, and JavaScript files.

   Using a CDN in front of your app can help with caching its pages and assets and serving them faster to users around the world. When a user requests a file, the CDN serves it from the server closest to the user, improving load times.

2. **Distributed Denial of Service (DDoS)** attacks are a common threat to web applications.

   Attackers send a large amount of traffic to your server, overwhelming it and making it unavailable to legitimate users. You can use a DDoS protection service in front of your app to protect it from these attacks.

We recommend using [Cloudflare](https://www.cloudflare.com/) for both CDN and DDoS protection. It's easy to set up and provides a free tier that should be enough for most small to medium-sized apps.

There are other CDN providers like [Fastly](https://www.fastly.com/), [Bunny](https://bunnycdn.com/) and [Amazon Cloudfront](https://aws.amazon.com/cloudfront/) that you can consider as well.

### Are Wasp apps production ready?

As we mentioned in the [introduction](./intro.md) section, a **Wasp app** is one server plus a database.

For the server, we are using Node.js with [Nitro](https://nitro.build/) and the battle-tested Express.js framework. For the database, we are using PostgreSQL, which is a powerful and reliable database system. For the pages, we are using React and Vite, which are both widely used and well-maintained.

Each of these pieces is production-ready on its own, and Wasp just makes it easy to connect them together. Keep in mind that Wasp is still considered beta software, so there might be some rough edges here and there.
