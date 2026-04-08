---
title: Extras
---

In this section, we will cover some additional topics that are important for deploying Wasp apps in production.

### Custom domain setup

If you want to set up a custom domain for your Wasp app, you can do it for both the client and the server.

The important part is setting up the custom domain for the client - that's what your users visit from their browsers. Setting up a custom domain for the server is optional, but it can be useful if you'd like to hide some server details (for example, the IP address or auto-generated domain name) from the users.

#### How to do it?

It's usually a two-step process, and it's the same for both the client and the server:

1. Set up the **DNS records** for the domain.

   This will depend on your hosting provider. You can usually do this by adding an `A` record in your DNS settings that points to the app's IPv4 address. You often set the `AAAA` record for IPv6 address as well. Some hosting providers ask you to set the `CNAME` record instead of the `A` and `AAAA` records.

:::note Using `wasp deploy`?

Check out how to set up custom domains with [Fly.io](./deployment-methods/wasp-deploy/fly.md#custom-domain) or [Railway](./deployment-methods/wasp-deploy/railway.md#custom-domain).

:::

2. Set up the **environment variables** for the app.

   You need to set the environment variables so Wasp configures the app correctly (for example, for CORS to work correctly).

   #### Client domain env vars

   When [building the client](./env-vars.md#client-env-vars), set `REACT_APP_API_URL` to point to your server domain:

   ```bash
   REACT_APP_API_URL=https://api.myapp.com
   ```

   <small>
     Learn more about client configuration in the [env vars section](../project/env-vars.md#client-general-configuration).
   </small>

   #### Server domain env vars

   For the server, you need to [configure two variables](./env-vars.md#server-env-vars):

   - `WASP_WEB_CLIENT_URL`: Your client app's domain
   - `WASP_SERVER_URL`: Your server domain

   <br />

   ```bash
   WASP_WEB_CLIENT_URL=https://myapp.com
   WASP_SERVER_URL=https://server.myapp.com
   ```

   <small>
     Learn more about server env variables in the [env vars section](../project/env-vars.md#server-general-configuration).
   </small>

### DDoS protection and CDN recommendations

When deploying your Wasp app, you might want to consider using a Content Delivery Network (CDN) and DDoS protection service to improve the performance and security of your app:

1. **Content Delivery Network (CDN)** is a network of servers distributed worldwide that caches static assets like images, CSS, and JavaScript files.

   Using a CDN in front of your **client** can help with caching static assets and serving them faster to users around the world. When a user requests a file, the CDN serves it from the server closest to the user, improving load times.

2. **Distributed Denial of Service (DDoS)** attacks are a common threat to web applications.

   Attackers send a large amount of traffic to your server, overwhelming it and making it unavailable to legitimate users. You can use a DDoS protection service for both your **client and server** to protect your app from these attacks.

We recommend using [Cloudflare](https://www.cloudflare.com/) for both CDN and DDoS protection. It's easy to set up and provides a free tier that should be enough for most small to medium-sized apps.

There are other CDN providers like [Fastly](https://www.fastly.com/), [Bunny](https://bunnycdn.com/) and [Amazon Cloudfront](https://aws.amazon.com/cloudfront/) that you can consider as well.

### Are Wasp apps production ready?

As we mentioned in the [introduction](./intro.md) section, what we call **Wasp apps** are three separate pieces: the client, the server, and the database.

For the server, we are using Node.js and the battle-tested Express.js framework. For the database, we are using PostgreSQL, which is a powerful and reliable database system. For the client, we are using React and Vite, which are both widely used and well-maintained.

Each of these pieces is production-ready on its own, and Wasp just makes it easy to connect them together. Keep in mind that Wasp is still considered beta software, so there might be some rough edges here and there.
