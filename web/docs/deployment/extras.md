---
title: Extras
---

In this section, we will cover some additional topics that are important for deploying Wasp apps in production.

### Custom domain setup

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

Since the server serves the client, your Wasp app lives on one domain, and that's the one your users visit from their browsers.

</TabItem>
<TabItem value="split" label="Split deployment">

The client and the server are hosted separately, so you can set up a custom domain for each of them. The important one is the client's, since that's what your users visit from their browsers. A custom domain for the server is optional, but it can be useful if you'd like to hide some server details (for example, the IP address or auto-generated domain name) from the users.

</TabItem>
</Tabs>

#### How to do it?

It's usually a two-step process:

1. Set up the **DNS records** for the domain.

   This will depend on your hosting provider. You can usually do this by adding an `A` record in your DNS settings that points to the app's IPv4 address. You often set the `AAAA` record for IPv6 address as well. Some hosting providers ask you to set the `CNAME` record instead of the `A` and `AAAA` records.

:::note Using `wasp deploy`?

Check out how to set up custom domains with [Fly.io](./deployment-methods/wasp-deploy/fly.md#custom-domain) or [Railway](./deployment-methods/wasp-deploy/railway.md#custom-domain).

:::

2. Set up the **environment variables** for the app.

   Wasp configures the app from these, for example when building OAuth redirect URIs, e-mail links and the CORS configuration.

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

Set [`WASP_SERVER_URL`](./env-vars.md#server-env-vars) to your domain. `WASP_WEB_CLIENT_URL` defaults to the same value:

```bash
WASP_SERVER_URL=https://myapp.com
```

If you use OAuth, update the redirect URI with your provider to `https://myapp.com/auth/<provider>/callback`.

</TabItem>
<TabItem value="split" label="Split deployment">

You have two domains, so [build the client](./env-vars.md#client-env-vars) with `REACT_APP_API_URL` pointing at the server's domain, and set both server variables:

```bash
REACT_APP_API_URL=https://api.myapp.com
```

```bash
WASP_WEB_CLIENT_URL=https://myapp.com
WASP_SERVER_URL=https://api.myapp.com
```

If you use OAuth, update the redirect URI with your provider to `https://api.myapp.com/auth/<provider>/callback`.

</TabItem>
</Tabs>

   <small>
     Learn more about server env variables in the [env vars section](../project/env-vars.md#server-general-configuration).
   </small>

### DDoS protection and CDN recommendations

When deploying your Wasp app, you might want to consider using a Content Delivery Network (CDN) and DDoS protection service to improve the performance and security of your app:

1. **Content Delivery Network (CDN)** is a network of servers distributed worldwide that caches static assets like images, CSS, and JavaScript files.

   Using a CDN in front of your **app** (or, in split mode, in front of your **client**) can help with caching static assets and serving them faster to users around the world. When a user requests a file, the CDN serves it from the server closest to the user, improving load times. Wasp serves the files under `/assets/` with immutable cache headers and the HTML without caching, so a CDN in front of the whole app does the right thing.

2. **Distributed Denial of Service (DDoS)** attacks are a common threat to web applications.

   Attackers send a large amount of traffic to your server, overwhelming it and making it unavailable to legitimate users. You can use a DDoS protection service in front of your **app** (in split mode, in front of both the **client and the server**) to protect it from these attacks.

We recommend using [Cloudflare](https://www.cloudflare.com/) for both CDN and DDoS protection. It's easy to set up and provides a free tier that should be enough for most small to medium-sized apps.

There are other CDN providers like [Fastly](https://www.fastly.com/), [Bunny](https://bunnycdn.com/) and [Amazon Cloudfront](https://aws.amazon.com/cloudfront/) that you can consider as well.

### Are Wasp apps production ready?

As we mentioned in the [introduction](./intro.md) section, what we call **Wasp apps** are three separate pieces: the client, the server, and the database.

For the server, we are using Node.js and the battle-tested Express.js framework. For the database, we are using PostgreSQL, which is a powerful and reliable database system. For the client, we are using React and Vite, which are both widely used and well-maintained.

Each of these pieces is production-ready on its own, and Wasp just makes it easy to connect them together. The server serving the client is plain Express static file serving, nothing exotic. Keep in mind that Wasp is still considered beta software, so there might be some rough edges here and there.
